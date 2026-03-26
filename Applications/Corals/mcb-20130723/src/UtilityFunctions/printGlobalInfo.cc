//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy by
// Lawrence Livermore National Laboratory under Contract DE-AC52-07NA27344

//  DISCLAIMER
//  This work was prepared as an account of work sponsored by an agency of the
//  United States Government. Neither the United States Government nor the
//  Lawrence Livermore National Security, LLC, nor any of their employees,
//  makes any warranty, express or implied, including the warranties of
//  merchantability and fitness for a particular purpose, or assumes any
//  legal liability or responsibility for the accuracy, completeness, or
//  usefulness of any information, apparatus, product, or process disclosed,
//  or represents that its use would not infringe privately owned rights.

#include "printGlobalInfo.hh"
#include "ASSERT.hh"
#include <limits>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <cstring>

namespace IMC_namespace
{

////////////////////////////////////////////////////////////////////////////////

void printGlobalInfo( std::vector< std::string >& tally_names,
                      std::vector< unsigned long long >& tallies,
                      int root,
                      bool print_all_processors )
{
   int num_procs = 1;
#ifdef USE_MPI
   int my_id = 0;
   MPI_Comm_rank( MPI_COMM_WORLD, &my_id);
   MPI_Comm_size( MPI_COMM_WORLD, &num_procs);
#endif

   ASSERT( tally_names.size() == tallies.size() );
   unsigned int size = tallies.size();

   // Get the maximum length of tally names
   unsigned int max_name_length = 0;
   for(unsigned int i=0; i<size; ++i)
   {
      if(tally_names[i].size()>max_name_length)
      {
         max_name_length = tally_names[i].size();
      }
   }

   // Do all kinds of fancy global processing if more than one processor
   if( num_procs > 1)
   {
#ifdef USE_MPI
      std::vector< unsigned long long > receive_buf;
      if( my_id == root )
      {
         receive_buf.resize( size * num_procs );
      }

      int isize = static_cast<int>(size);
      MPI_Gather( &tallies[0], isize, MPI_UNSIGNED_LONG_LONG,
                  &receive_buf[0], isize, MPI_UNSIGNED_LONG_LONG,
                  root, MPI_COMM_WORLD );

      if( my_id == root )
      {
         std::vector< unsigned long long > t_min(size);
         std::vector< unsigned long long > t_max(size);
         std::vector< unsigned long long > t_sum(size);

         // Compute the maximum, minimum, and total
         for(unsigned int i=0; i<size; ++i)
         {
            t_min[i] = std::numeric_limits<unsigned long long>::max();
            t_max[i] = std::numeric_limits<unsigned long long>::min();
            t_sum[i] = 0;

            for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
            {
               unsigned int index = i+j*size;
               unsigned long long value = receive_buf[index];

               t_sum[i] += value;

               if( value > t_max[i] )
               {
                  t_max[i] = value;
               }
               if( value < t_min[i] )
               {
                  t_min[i] = value;
               }
            }

         }

         std::vector< double > t_avg(size);
         std::vector< double > t_sum_sq(size);
         std::vector< double > t_std_dev(size);

         // Compute the average and standard deviation
         for(unsigned int i=0; i<size; ++i)
         {
            t_avg[i] = static_cast<double>(t_sum[i]) / static_cast<double>(num_procs);

            t_sum_sq[i] = 0.0;

            for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
            {
               unsigned int index = i+j*size;
               unsigned long long value = receive_buf[index];

               t_sum_sq[i] += (value-t_avg[i])*(value-t_avg[i]);

            }

            double denom = num_procs * static_cast<double>(num_procs-1);
            t_std_dev[i] = std::sqrt( t_sum_sq[i] / denom );
         }

         unsigned int int_width = std::numeric_limits<unsigned long long>::digits10+3;

         std::ios::fmtflags flags(std::cout.flags());
         unsigned int precision(std::cout.precision());

         std::cout << std::setiosflags(std::ios::scientific);
         unsigned int new_precision = 10;
         std::cout << std::setprecision(new_precision);
         unsigned int double_width = new_precision+9;

         unsigned int total_width = max_name_length+3+int_width+4*double_width;

         std::cout 
            << std::setw( max_name_length +3)
            << "Tally"
            << std::setw( int_width )
            << "Global_Sum"
            << std::setw( double_width )
            << "Average"
            << std::setw( double_width )
            << "Standard_deviation"
            << std::setw( double_width )
            << "Minimum"
            << std::setw( double_width )
            << "Maximum";

         if( print_all_processors )
         {
            total_width += int_width*num_procs;
            for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
            {
               std::ostringstream out;
               out << "Processor_" << j;
               std::cout
                  << std::setw( int_width )
                  << out.str();
            }
         }
         std::cout << '\n';

         for(unsigned int i=0; i<total_width; ++i)
         {
            std::cout << '-';
         }
         std::cout << '\n';

         // Print out info
         for(unsigned int i=0; i<size; ++i)
         {
            std::cout 
               << std::setw( max_name_length +3)
               << tally_names[i]
               << std::setw( int_width )
               << t_sum[i]
               << std::setw( double_width )
               << t_avg[i]
               << std::setw( double_width )
               << t_std_dev[i]
               << std::setw( double_width )
               << t_min[i]
               << std::setw( double_width )
               << t_max[i];

            if( print_all_processors )
            {
               for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
               {
                  unsigned int index = i+j*size;
                  unsigned long long value = receive_buf[index];
                  std::cout
                     << std::setw( int_width )
                     << value;
               }
            }
            std::cout << '\n';
         }

         std::cout.flags(flags);
         std::cout.precision(precision);

      }
#endif
   }
   else
   {

      unsigned int number_width = std::numeric_limits<unsigned long long>::digits10+3;

      std::cout
         << std::setw( max_name_length +3)
         << "Tally"
         << std::setw( number_width )
         << "Count"
         << '\n';

      for(unsigned int i=0; i<max_name_length+3+number_width; ++i)
      {
         std::cout << '-';
      }
      std::cout << '\n';

      // Print out single-processor info
      for(unsigned int i=0; i<size; ++i)
      {
         std::cout << std::setw( max_name_length +3)
            << tally_names[i]
            << std::setw( number_width )
            << tallies[i]
            << '\n';
      }
   }
}



void printGlobalInfoPrintf( std::vector< std::string >& tally_names,
                            std::vector< unsigned long long >& tallies,
                            int root,
                            bool print_all_processors )
{
    int num_procs = 1;
#ifdef USE_MPI
    int my_id = 0;
    MPI_Comm_rank( MPI_COMM_WORLD, &my_id);
    MPI_Comm_size( MPI_COMM_WORLD, &num_procs);
#endif

    ASSERT( tally_names.size() == tallies.size() );
    unsigned int size = tallies.size();
    
    // Get the maximum length of tally names
    unsigned int max_name_length = 0;
    for(unsigned int i=0; i<size; ++i)
    {
        if(tally_names[i].size()>max_name_length)
        {
            max_name_length = tally_names[i].size();
        }
    }
    max_name_length += 3;
    
    // Do all kinds of fancy global processing if more than one processor
    if( num_procs > 1)
    {
#ifdef USE_MPI
        std::vector< unsigned long long > receive_buf;
        if( my_id == root )
        {
            receive_buf.resize( size * num_procs );
        }

        int isize = static_cast<int>(size);
        MPI_Gather( &tallies[0], isize, MPI_UNSIGNED_LONG_LONG,
                    &receive_buf[0], isize, MPI_UNSIGNED_LONG_LONG,
                    root, MPI_COMM_WORLD );

        if( my_id == root )
        {
            std::vector< unsigned long long > t_min(size);
            std::vector< unsigned long long > t_max(size);
            std::vector< unsigned long long > t_sum(size);

            // Compute the maximum, minimum, and total
            for(unsigned int i=0; i<size; ++i)
            {
                t_min[i] = std::numeric_limits<unsigned long long>::max();
                t_max[i] = std::numeric_limits<unsigned long long>::min();
                t_sum[i] = 0;

                for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
                {
                    unsigned int index = i+j*size;
                    unsigned long long value = receive_buf[index];

                    t_sum[i] += value;

                    if( value > t_max[i] )
                    {
                        t_max[i] = value;
                    }
                    if( value < t_min[i] )
                    {
                        t_min[i] = value;
                    }
                }

            }

            std::vector< double > t_avg(size);
            std::vector< double > t_sum_sq(size);
            std::vector< double > t_std_dev(size);

            // Compute the average and standard deviation
            for(unsigned int i=0; i<size; ++i)
            {
                t_avg[i] = static_cast<double>(t_sum[i]) / static_cast<double>(num_procs);

                t_sum_sq[i] = 0.0;

                for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
                {
                    unsigned int index = i+j*size;
                    unsigned long long value = receive_buf[index];

                    t_sum_sq[i] += (value-t_avg[i])*(value-t_avg[i]);

                }

                double denom = num_procs * static_cast<double>(num_procs-1);
                t_std_dev[i] = std::sqrt( t_sum_sq[i] / denom );
            }

            unsigned int int_width = std::numeric_limits<unsigned long long>::digits10+3;

            unsigned int new_precision = 10;
            unsigned int double_width = new_precision+9;

            unsigned int total_width = max_name_length+int_width+4*double_width;

            for(int i=0;i<max_name_length-5;i++)printf(" ");
            printf("Tally");
            for(int i=0;i<int_width-10;i++)printf(" ");
            printf("Global_sum");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Average");
            for(int i=0;i<double_width-18;i++)printf(" ");
            printf("Standard_deviation");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Minimum");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Maximum");
            
            if( print_all_processors )
            {
                total_width += int_width*num_procs;
                for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
                {
                    for(int i=0;i<int_width-11;i++) printf(" ");
                    printf("Processor_%u",j);
                }
            }
            printf("\n");

            for(unsigned int i=0; i<total_width; ++i)
            {
                printf("-");
            }
            printf("\n");

            int zero=0;
            
            // Print out info
            for(unsigned int j=0; j<size; ++j)
            {
                for(int i=0;i<max_name_length - strlen(tally_names[j].c_str());i++)printf(" ");
                printf("%s",tally_names[j].c_str());
                (t_sum[j]==0)?printf("%*.c",int_width,'0'):printf("%*.llu",int_width,t_sum[j]);
                printf("%*.*e",double_width, new_precision, t_avg[j]);
                printf("%*.*e",double_width, new_precision, t_std_dev[j]);
                (t_min[j]==0)?printf("%*.c",double_width,'0'):printf("%*.llu",double_width, t_min[j]);
                (t_max[j]==0)?printf("%*.c",double_width,'0'):printf("%*.llu",double_width, t_max[j]);
                
                if( print_all_processors )
                {
                    for(unsigned int i=0; i<static_cast<unsigned int>(num_procs); ++i)
                    {
                        unsigned int index = j+i*size;
                        unsigned long long value = receive_buf[index];
                        printf("%*.llu",int_width, value);
                    }
                }
                printf("\n");
            }
        }
#endif
    }
    else
    {
        // only one MPI task, simple output
        unsigned int number_width = std::numeric_limits<unsigned long long>::digits10+3;

        printf("%21s %22s\n","Tally","Count");

        for(unsigned int i=0; i<max_name_length+number_width; ++i)
        {
            printf("-");
        }
        printf("\n");
        // Print out single-processor info
        for(unsigned int i=0; i<size; ++i)
        {
            printf("%21s %22llu\n",tally_names[i].c_str(),tallies[i]);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////

void printGlobalInfo( std::vector< std::string >& tally_names,
                      std::vector< double >& tallies,
                      int root,
                      bool print_all_processors )
{
   int num_procs = 1;
#ifdef USE_MPI
   int my_id = 0;
   MPI_Comm_rank( MPI_COMM_WORLD, &my_id);
   MPI_Comm_size( MPI_COMM_WORLD, &num_procs);
#endif

   ASSERT( tally_names.size() == tallies.size() );
   unsigned int size = tallies.size();

   std::ios::fmtflags flags(std::cout.flags());
   unsigned int precision(std::cout.precision());

   std::cout << std::setiosflags(std::ios::scientific);
   unsigned int new_precision = 10;
   std::cout << std::setprecision(new_precision);
   unsigned int double_width = new_precision+9;

   // Get the maximum length of tally names
   unsigned int max_name_length = 0;
   for(unsigned int i=0; i<size; ++i)
   {
      if(tally_names[i].size()>max_name_length)
      {
         max_name_length = tally_names[i].size();
      }
   }

   // Do all kinds of fancy global processing if more than one processor
   if( num_procs > 1)
   {
#ifdef USE_MPI
      std::vector< double > receive_buf;
      if( my_id == root )
      {
         receive_buf.resize( size * num_procs );
      }

      int isize = static_cast<int>(size);
      MPI_Gather( &tallies[0],
                  isize,
                  MPI_DOUBLE,
                  &receive_buf[0],
                  isize,
                  MPI_DOUBLE,
                  root,
                  MPI_COMM_WORLD );

      if( my_id == root )
      {
         std::vector< double > t_min(size);
         std::vector< double > t_max(size);
         std::vector< double > t_sum(size);

         // Compute the maximum, minimum, and total
         for(unsigned int i=0; i<size; ++i)
         {
            t_min[i] = std::numeric_limits<double>::max();
            t_max[i] = -std::numeric_limits<double>::max();
            t_sum[i] = 0;

            for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
            {
               unsigned int index = i+j*size;
               double value = receive_buf[index];

               t_sum[i] += value;

               if( value > t_max[i] )
               {
                  t_max[i] = value;
               }
               if( value < t_min[i] )
               {
                  t_min[i] = value;
               }
            }

         }

         std::vector< double > t_avg(size);
         std::vector< double > t_sum_sq(size);
         std::vector< double > t_std_dev(size);

         // Compute the average and standard deviation
         for(unsigned int i=0; i<size; ++i)
         {
            t_avg[i] = t_sum[i] / num_procs;

            t_sum_sq[i] = 0.0;

            for(unsigned int j=0; j < static_cast<unsigned int>(num_procs); ++j)
            {
               unsigned int index = i+j*size;
               double value = receive_buf[index];

               t_sum_sq[i] += ( value - t_avg[i] ) * ( value - t_avg[i] );

            }

            double denom = num_procs * static_cast<double>(num_procs-1);
            t_std_dev[i] = std::sqrt( t_sum_sq[i] / denom );
         }

         unsigned int total_width = max_name_length + 3 + 5*double_width;

         std::cout 
            << std::setw( max_name_length +3)
            << "Tally"
            << std::setw( double_width )
            << "Global_Sum"
            << std::setw( double_width )
            << "Average"
            << std::setw( double_width )
            << "Standard_deviation"
            << std::setw( double_width )
            << "Minimum"
            << std::setw( double_width )
            << "Maximum";

         if( print_all_processors )
         {
            total_width += double_width*num_procs;
            for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
            {
               std::ostringstream out;
               out << "Processor_" << j;
               std::cout
                  << std::setw( double_width )
                  << out.str();
            }
         }
         std::cout << '\n';

         for(unsigned int i=0; i<total_width; ++i)
         {
            std::cout << '-';
         }
         std::cout << '\n';

         // Print out info
         for(unsigned int i=0; i<size; ++i)
         {
            std::cout 
               << std::setw( max_name_length +3)
               << tally_names[i]
               << std::setw( double_width )
               << t_sum[i]
               << std::setw( double_width )
               << t_avg[i]
               << std::setw( double_width )
               << t_std_dev[i]
               << std::setw( double_width )
               << t_min[i]
               << std::setw( double_width )
               << t_max[i];

            if( print_all_processors )
            {
               for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
               {
                  unsigned int index = i+j*size;
                  double value = receive_buf[index];
                  std::cout
                     << std::setw( double_width )
                     << value;
               }
            }
            std::cout << '\n';
         }

      }
#endif
   }
   else
   {
      std::cout
         << std::setw( max_name_length +3)
         << "Tally"
         << std::setw( double_width )
         << "Value"
         << '\n';

      for(unsigned int i=0; i<max_name_length+3+double_width; ++i)
      {
         std::cout << '-';
      }
      std::cout << '\n';

      // Print out single-processor info
      for(unsigned int i=0; i<size; ++i)
      {
         std::cout << std::setw( max_name_length +3)
            << tally_names[i]
            << std::setw( double_width )
            << tallies[i]
            << '\n';
      }
   }

   std::cout.flags(flags);
   std::cout.precision(precision);

}

////////////////////////////////////////////////////////////////////////////////

void printGlobalInfoPrintf( std::vector< std::string >& tally_names,
                            std::vector< double >& tallies,
                            int root,
                            bool print_all_processors )
{

    int num_procs = 1;
#ifdef USE_MPI
    int my_id = 0;
    MPI_Comm_rank( MPI_COMM_WORLD, &my_id);
    MPI_Comm_size( MPI_COMM_WORLD, &num_procs);
#endif

    ASSERT( tally_names.size() == tallies.size() );
    unsigned int size = tallies.size();

    // Get the maximum length of tally names
    unsigned int max_name_length = 0;
    for(unsigned int i=0; i<size; ++i)
    {
        if(tally_names[i].size()>max_name_length)
        {
            max_name_length = tally_names[i].size();
        }
    }
    max_name_length += 3;  // (give a little space past name)

    unsigned int new_precision = 10;
    unsigned int double_width = new_precision+9;


    // Do all kinds of fancy global processing if more than one processor
    if( num_procs > 1)
    {
#ifdef USE_MPI
        std::vector< double > receive_buf;
        if( my_id == root )
        {
            receive_buf.resize( size * num_procs );
        }

        int isize = static_cast<int>(size);
        MPI_Gather( &tallies[0],
                    isize,
                    MPI_DOUBLE,
                    &receive_buf[0],
                    isize,
                    MPI_DOUBLE,
                    root,
                    MPI_COMM_WORLD );

        if( my_id == root )
        {
            std::vector< double > t_min(size);
            std::vector< double > t_max(size);
            std::vector< double > t_sum(size);

            // Compute the maximum, minimum, and total
            for(unsigned int i=0; i<size; ++i)
            {
                t_min[i] = std::numeric_limits<double>::max();
                t_max[i] = -std::numeric_limits<double>::max();
                t_sum[i] = 0;

                for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
                {
                    unsigned int index = i+j*size;
                    double value = receive_buf[index];

                    t_sum[i] += value;

                    if( value > t_max[i] )
                    {
                        t_max[i] = value;
                    }
                    if( value < t_min[i] )
                    {
                        t_min[i] = value;
                    }
                }

            }

            std::vector< double > t_avg(size);
            std::vector< double > t_sum_sq(size);
            std::vector< double > t_std_dev(size);

            // Compute the average and standard deviation
            for(unsigned int i=0; i<size; ++i)
            {
                t_avg[i] = t_sum[i] / num_procs;

                t_sum_sq[i] = 0.0;

                for(unsigned int j=0; j < static_cast<unsigned int>(num_procs); ++j)
                {
                    unsigned int index = i+j*size;
                    double value = receive_buf[index];

                    t_sum_sq[i] += ( value - t_avg[i] ) * ( value - t_avg[i] );

                }

                double denom = num_procs * static_cast<double>(num_procs-1);
                t_std_dev[i] = std::sqrt( t_sum_sq[i] / denom );
            }

            unsigned int total_width = max_name_length + 3 + 5*double_width;

            for(int i=0;i<max_name_length-5;i++)printf(" ");
            printf("Tally");
            for(int i=0;i<double_width-10;i++)printf(" ");
            printf("Global_sum");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Average");
            for(int i=0;i<double_width-18;i++)printf(" ");
            printf("Standard_deviation");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Minimum");
            for(int i=0;i<double_width-7;i++)printf(" ");
            printf("Maximum");
            
            if( print_all_processors )
            {
                total_width += double_width*num_procs;
                for(unsigned int j=0; j<static_cast<unsigned int>(num_procs); ++j)
                {
                    for(int i=0;i<double_width-11;i++) printf(" "); // for more than 9 procs this will be off by one
                    printf("Processor_%u",j);
                }
            }
            printf("\n");

            for(unsigned int i=0; i<total_width; ++i)
            {
                printf("-");
            }
            printf("\n");

            // Print out info
            for(unsigned int j=0; j<size; ++j)
            {
                for(int i=0;i<max_name_length - strlen(tally_names[j].c_str());i++)printf(" ");
                printf("%s",tally_names[j].c_str());
                printf("%*.*e",double_width, new_precision, t_sum[j]);
                printf("%*.*e",double_width, new_precision, t_avg[j]);
                printf("%*.*e",double_width, new_precision, t_std_dev[j]);
                printf("%*.*e",double_width, new_precision, t_min[j]);
                printf("%*.*e",double_width, new_precision, t_max[j]);

                if( print_all_processors )
                {
                    for(unsigned int i=0; i<static_cast<unsigned int>(num_procs); ++i)
                    {
                        unsigned int index = j+i*size;
                        double value = receive_buf[index];
                        printf("%*.*e",double_width, new_precision, value);
                    }
                }
                printf("\n");
            }

        }
#endif
    }
    else
    {
        //
        // single mpi task case
        //
        for(int j=0;j<max_name_length-5;++j)printf(" ");
        printf("Tally");
        for(int j=0;j<double_width - 5;++j)printf(" ");
        printf("Value\n");
      
        for(unsigned int i=0; i<max_name_length+double_width; ++i)
        {
            printf("-");

        }
        printf("\n");

        // Print out single-processor info
        for(unsigned int i=0; i<size; ++i)
        {
            for(int j=0;j<max_name_length-tally_names[i].size();++j)printf(" ");
//             printf("%*.s",max_name_length,tally_names[i].c_str());
            printf("%s",tally_names[i].c_str());
            printf("%*.*e\n",double_width, new_precision, tallies[i]);
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

} // IMC_namespace

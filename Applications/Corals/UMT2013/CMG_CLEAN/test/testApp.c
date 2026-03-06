/*
  A simple test application that will ask for input and then format the string
  in a format that is readible by the compact mesh generator
*/


#include <string.h>
#include <stdlib.h>

#include "cmg.h"
#include "cmgConstants.h"
#include "CMGDebug.h"
#ifdef USE_MPI
#include "mpi.h"
#endif

void readInput( FILE* );

int main(int argc, char* argv[])
{
    /*Read input from a file and parse it*/
  FILE *inputDeck;
  char fileName[100];
  char output[100];
  int ii,jj,kk;
  int counter = 0;
  
  bool inputFileSet = false;
  bool outputFileSet = false;
  int myRank = 0;
#ifdef USE_MPI
  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

  CMGDPRINT("Processor id %d reporting in\n",myRank);
  #endif
  

  
  
  
  if(myRank==0){

      /*If I am process 0 read the input and parse it, only
        once
      */
    while(counter<argc-1){
        /*parse the input file*/
      if(strcmp(argv[counter],"-i")==0) {
        strcpy(fileName, argv[counter+1]);
        inputFileSet = true;
      }
      else if(strcmp(argv[counter],"-o")==0) {
        strcpy(output,argv[counter +1]);
        outputFileSet = true;
      }
    
      counter++;
    
    }

    if (inputFileSet) {
      CMGDPRINT("File name read is: %s\n",fileName);
        /*Open the file to parse */
      inputDeck = fopen(fileName, "r");
    }
    else {
      CMGDPRINT("No input file specified, reading from stdin. Finish input with ^D.\n");
      inputDeck = NULL;
    }
      /*print the prompt*/
    CMGDPRINT("CMG > ");

  
    readInput(inputDeck);
 
 

    if (inputFileSet) {
        /*close the file */
      int success = fclose(inputDeck);
    }

   
  
  }
  
  bcastinp( );
   
  cgenmesh();

    /*Lets query the mesh data here and see how it looks*/
  
  int totalzones,totalnodes;

  cnumnod(&totalnodes);

  CMGDPRINT("Total nodes on domain %d: %d\n",myRank,totalnodes);

  double *x = (double*)malloc(sizeof(double) * totalnodes);
  double *y = (double*)malloc(sizeof(double) * totalnodes);
  double *z = (double*)malloc(sizeof(double) * totalnodes);
  int *nodeList = (int*)malloc(sizeof(int) *totalnodes);

  cnods(nodeList);
  cndspos(x,y,z);
  
  for(ii=0;ii<totalnodes;++ii)
      CMGDPRINT("Domain: %d Node Id: %d, x:%f,y:%f,z:%f\n",myRank,nodeList[ii],x[ii],y[ii],z[ii]);
  
  free(x);
  free(y);
  free(z);
  free(nodeList);
  
  
  cnumzns(&totalzones);

  CMGDPRINT("Total zones on domain %d: %d\n",myRank,totalzones);

  int *zones = (int*)malloc(sizeof(int) * totalzones);
  czns(zones);
  
  
  for(ii=0;ii<totalzones;ii++){
    cZoneType zoneType;
    cgetztp(zones[ii],&zoneType);
    
    CMGDPRINT("ZoneType: %d Nodes: ",zoneType);
    int *nodeIds = (int*)malloc(zoneType*sizeof(int));
    
    cgetznn(zones[ii],nodeIds);
    for(jj=0;jj<zoneType;jj++)
        CMGDPRINT("%d ",nodeIds[jj]);

    free(nodeIds);
    
    CMGDPRINT("\n");  
  }
  
  free(zones);
  
  
  int numMeshTags = -1;
  cmtgs(&numMeshTags);
  CMGDPRINT("Domain: %d, Found %d meshTags in test\n",myRank,numMeshTags);
 

  for( ii = 0; ii< numMeshTags;++ii){
    char tagName[100];
    int tagType;
    int idSize=0;
    int *ids;
    cmtgnm(ii,tagName);
    CMGDPRINT("Domain: %d, Tag Name: %s\n",myRank,tagName);
    cmtgtp(ii,&tagType);
    CMGDPRINT("Domain: %d, Tag type: %d\n",myRank,tagType);
    cmtgidsz(ii,&idSize);
    CMGDPRINT("Domain: %d, Number of ids: %d\n",myRank,idSize);
    ids = (int*)malloc(sizeof(int) * idSize);
    cmtgid(ii,ids);
    if(tagType == 3 || tagType == 4){
      int zz,yy;
      for(zz=0;zz<idSize;++zz){
          /*double check the ids back to tags
           */
        int currentId = ids[zz];
        int tagListSize = 0;
        cmtgfzsz(currentId,&tagListSize);
        int *tagIdsFromZone = malloc(sizeof(int) * tagListSize);
        cmtgfz(currentId,tagIdsFromZone);
        for(yy = 0;yy<tagListSize;++yy){
          CMGDPRINT("Domain: %d, Zone Id: %d TagId: %d\n",myRank,currentId,tagIdsFromZone[yy]);
        }
        free(tagIdsFromZone);
      }
    }
    
    for(jj=0;jj<idSize;++jj){
      CMGDPRINT("Domain: %d, id: %d\n",myRank,ids[jj]);
      if(tagType==0){
        double x,y,z;
        cnodpos(ids[jj],&x,&y,&z);
        CMGDPRINT("Domain: %d,  x:%f y:%f z:%f\n",myRank,x,y,z);
      }
      if(tagType==2){
        int *nodeIds = (int*)malloc(sizeof(int) *4);
        cfcnda(ids[jj],nodeIds);
        CMGDPRINT("Domain: %d, NodeIds for Face Tag %s: %d %d %d %d\n",myRank,tagName,
                  nodeIds[0],nodeIds[1],nodeIds[2],nodeIds[3]);
        
        free(nodeIds);
      }
    }
    CMGDPRINT("\n");
    
    free(ids);
  }
 
  
    /*Lets print out the information for send and recieve*/
  #ifdef USE_MPI
  int domainId;
  MPI_Comm_rank(MPI_COMM_WORLD,&domainId);
  MPI_Barrier(MPI_COMM_WORLD);
  CMGDPRINT("Printing out Domain specific information\n");
  
  int boundaryFaces;

  cnumbdf(&boundaryFaces);

  CMGDPRINT("Number of Boundary Faces on domain: %d are: %d\n",domainId,boundaryFaces);

  int *boundaryFaceList = (int*)malloc(sizeof(int) * boundaryFaces);

  cbdrfc(boundaryFaceList);

  for(jj=0;jj<boundaryFaces;++jj){
    int *nodeIds = (int*)malloc(sizeof(int) * 4);
    cfcnda(boundaryFaceList[jj],nodeIds);
    CMGDPRINT("FaceId: %d NodeList:",boundaryFaceList[jj]);
    for(ii = 0;ii<4;++ii){
      double x,y,z;
      cnodpos(nodeIds[ii],&x,&y,&z);
      CMGDPRINT(" %d(%f,%f,%f) ",nodeIds[ii],x,y,z);  
    }
    CMGDPRINT("\n");      
    free(nodeIds);
  }
  free(boundaryFaceList);
  
  int totalNodes;

  cnmnoda(&totalNodes);
  CMGDPRINT("Total Nodes on this domain(owned or not): %d\n",totalNodes);

  int *totalNodesArray = (int*)malloc(sizeof(int) * totalNodes);
  int offset;
  cnodsa(totalNodesArray,&offset);

  CMGDPRINT("Offset is: %d\n",offset);

  CMGDPRINT("NodesArray: ");
  for(jj=0;jj<totalNodes;++jj){
    CMGDPRINT("Domain %d, Node %d\n",domainId,totalNodesArray[jj]);
  }
  free(totalNodesArray);
  
  
 

    /*The code below works, it is just commented out so the print out
      on the screen is not so verbose*/
  
    /* int size; */

/*     crecfsz(&size); */
/*     int *domainArray = (int*)malloc(sizeof(int) * size); */
/*     int *faceSizeArray = (int*)malloc(sizeof(int) *size); */

/*     crecfar(domainArray,faceSizeArray); */

/*     for(jj=0;jj<size;++jj){ */
/*       int *faceIds = (int*)malloc(sizeof(int) * faceSizeArray[jj]); */
/*       crecf(domainArray[jj],faceIds); */
/*       CMGDPRINT("DomainId:%d Recieve Faces:",domainId); */
/*       for(kk=0;kk<faceSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ",faceIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(faceIds); */
/*     } */
/*     free(domainArray); */
/*     free(faceSizeArray); */


/*     csndfsz(&size); */
/*     domainArray = (int*)malloc(sizeof(int) *size); */
/*     faceSizeArray = (int*)malloc(sizeof(int) * size); */

/*     csndfar(domainArray,faceSizeArray); */

/*     for(jj=0;jj<size;++jj){ */
/*       int *faceIds = (int*)malloc(sizeof(int) * faceSizeArray[jj]); */
/*       csndf(domainArray[jj],faceIds); */
/*       CMGDPRINT("DomainId:%d Send Faces:",domainId); */
/*       for(kk=0;kk<faceSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ",faceIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(faceIds); */
/*     } */
/*     free(domainArray); */
/*     free(faceSizeArray); */

/*     creczsz(&size); */
/*     domainArray = (int*)malloc(sizeof(int) *size); */
/*     int *zoneSizeArray = (int*)malloc(sizeof(int) * size); */

/*     creczar(domainArray,zoneSizeArray); */
/*     for(jj=0;jj<size;++jj){ */
/*       int *zoneIds = (int*)malloc(sizeof(int) * zoneSizeArray[jj]); */
/*       crecz(domainArray[jj],zoneIds); */
/*       CMGDPRINT("DomainId:%d RecieveZones:",domainId); */
/*       for(kk=0;kk<zoneSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ", zoneIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(zoneIds); */
/*     } */

/*     free(domainArray); */
/*     free(zoneSizeArray); */


/*     csndzsz(&size); */
/*     domainArray = (int*)malloc(sizeof(int) *size); */
/*     zoneSizeArray = (int*)malloc(sizeof(int) * size); */

/*     csndzar(domainArray,zoneSizeArray); */
/*     for(jj=0;jj<size;++jj){ */
/*       int *zoneIds = (int*)malloc(sizeof(int) * zoneSizeArray[jj]); */
/*       csndz(domainArray[jj],zoneIds); */
/*       CMGDPRINT("DomainId:%d SendZones:",domainId); */
/*       for(kk=0;kk<zoneSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ", zoneIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(zoneIds); */
/*     } */

/*     free(domainArray); */
/*     free(zoneSizeArray); */


/*     crecnsz(&size); */
/*     domainArray = (int*)malloc(sizeof(int) *size); */
/*     int *nodeSizeArray = (int*)malloc(sizeof(int) * size); */

/*     crecnar(domainArray,nodeSizeArray); */
/*     for(jj=0;jj<size;++jj){ */
/*       int *nodeIds = (int*)malloc(sizeof(int) * nodeSizeArray[jj]); */
/*       crecn(domainArray[jj],nodeIds); */
/*       CMGDPRINT("DomainId:%d RecieveNodes:",domainId); */
/*       for(kk=0;kk<nodeSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ", nodeIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(nodeIds); */
/*     } */

/*     free(domainArray); */
/*     free(nodeSizeArray); */


/*     csndnsz(&size); */
/*     domainArray = (int*)malloc(sizeof(int) *size); */
/*     nodeSizeArray = (int*)malloc(sizeof(int) * size); */

/*     csndnar(domainArray,nodeSizeArray); */
/*     for(jj=0;jj<size;++jj){ */
/*       int *nodeIds = (int*)malloc(sizeof(int) * nodeSizeArray[jj]); */
/*       csndn(domainArray[jj],nodeIds); */
/*       CMGDPRINT("DomainId:%d SendNodes:",domainId); */
/*       for(kk=0;kk<nodeSizeArray[jj];++kk){ */
/*         CMGDPRINT(" %d ", nodeIds[kk]); */
/*       } */
/*       CMGDPRINT("\n"); */
/*       free(nodeIds); */
/*     } */

/*     free(domainArray); */
/*     free(nodeSizeArray);  */
  
  
  #endif
 
  


  CMGDPRINT("Finishing Test Program\n");

    /* Clean up all allocated mesh data */
  cclnmd( );

  #ifdef USE_MPI
  MPI_Finalize( );
  #endif
  
  return 0;
  

}

void readInput(FILE *input)
{
  
/*   int c; */
/*   int counter = 0; */
/*   char l[CMG_MAX_LINE]; */

    /*this could read in everything or just one line at at time */
  
/*   while(fgets(l,CMG_MAX_LINE,input)){ */
    
/*     CMGDPRINT("Reading Line from file:\n"); */
/*     CMGDPRINT("%s\n",l); */


      /*send line off to command parser */
  /*  } */
  
  if (input != NULL) {
    /* Assign the file to the variable that is read by the parser */
    csetinp( input );
  }
  else {
    /*Leave it set to stdin */
  }

  /* Call the CMG parsing routine */
  creadinp();
  /* Call the CMG input printing routine */
  cprntinp();

}



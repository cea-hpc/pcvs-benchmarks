#include <iostream>
#include "geom/CMI/MeshBase.hh"
#include "geom/Field/Field.hh"
#include "mpi.h"

void foo(const Geometry::MeshBase& M);

int main(int argc, char* argv[])
{
    int myRank, numProcs;
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
    MPI_Comm_size(MPI_COMM_WORLD,&numProcs);
    
    std::cout<<"my proc is "<<myRank<<" out of "<<numProcs<<std::endl;
    
    //
    // build me a MeshBase
    Geometry::MeshBase myMesh( "problem2.cmg" );
    
    //
    // iterate over nodes
    std::vector<double> position;
    
#if 1
    for(Geometry::MeshBase::NodeIterator nodeIt = myMesh.nodeBegin();
        nodeIt != myMesh.nodeEnd(); ++nodeIt)
    {
        Geometry::NodeBase theNode = *nodeIt;
        position = theNode.getPosition();
        std::cout<<"proc "<<myRank<<" node IDs (local,global)= ("<<theNode.getLocalID()<<","<<theNode.getGlobalID()<<").  node position = ["<<position[0]<<", "<<position[1]<<", "<<position[2]<<"]."<<std::endl;
    }
    std::cout<<std::endl<<std::endl;

    //
    // iterate over faces
    //
    for(Geometry::MeshBase::FaceIterator faceIt = myMesh.faceBegin();
        faceIt != myMesh.faceEnd();
        ++faceIt)
    {
        const Geometry::FaceBase &theFace = *faceIt;
        position = theFace.getPosition();
        std::string theFaceTagName = theFace.getTag();
        std::cout<<"proc "<<myRank<<" face IDs (local,global)= ("<<theFace.getLocalID()<<","<<theFace.getGlobalID()<<").";
        std::cout<<"  face centroid = ["<<position[0]<<", "<<position[1]<<", "<<position[2]<<"],";
        std::cout<<"  classification: "<<theFace.getClassification()<<"  Tag: "<<theFaceTagName<<std::endl;
     }
    std::cout<<std::endl<<std::endl;
    
    std::cout<<"proc "<<myRank<<" number of faces = "<<myMesh.getNumberOfFaces()<<" number of owned zones = "<<myMesh.getNumberOfOwnedZones()<<" number of owned nodes = "<<myMesh.getNumberOfOwnedNodes()<<std::endl;
#endif

    MPI_Barrier( MPI_COMM_WORLD );
    
#if 1
    // -----------------------------------------------------
    // communication data test
    // -----------------------------------------------------

    const Communication::CommAgent &theCommAgent = myMesh.getCommAgent();
    const Communication::CommAgent::FaceMap &theFaceNbrMap = theCommAgent.getFaceNeighborMap();

    std::vector<Geometry::FaceBase>::const_iterator recvElemIter,sendElemIter;
    std::set<size_t>::const_iterator recvProcIter,sendProcIter;
    
    
    for(sendProcIter = theFaceNbrMap.sendProcessBegin();
        sendProcIter != theFaceNbrMap.sendProcessEnd();
        ++sendProcIter)
    {
        
        for(sendElemIter = theFaceNbrMap.sendElementBegin(*sendProcIter);
            sendElemIter != theFaceNbrMap.sendElementEnd(*sendProcIter);
            ++sendElemIter)
        {
            position = sendElemIter->getPosition();

            std::cout<<"proc "<<myRank<<" send procID: "<<*sendProcIter <<" face Local ID: "<< sendElemIter->getLocalID()<<"  Global ID: "<<sendElemIter->getGlobalID();
            std::cout<<" face centroid = ["<<position[0]<<", "<<position[1]<<", "<<position[2]<<"],";
            std::cout<<" opp face has classification: "<<sendElemIter->getOppositeFace().getClassification()<<std::endl;
        }
    }
    
    for(recvProcIter = theFaceNbrMap.receiveProcessBegin();
        recvProcIter != theFaceNbrMap.receiveProcessEnd();
        ++recvProcIter)
    {
        
        for(recvElemIter = theFaceNbrMap.receiveElementBegin(*recvProcIter);
            recvElemIter != theFaceNbrMap.receiveElementEnd(*recvProcIter);
            ++recvElemIter)
        {
            position = recvElemIter->getPosition();
            std::cout<<"proc "<<myRank<<" receive procID: "<< *recvProcIter<<" face Local ID: "<< recvElemIter->getLocalID()<<"  Global ID: "<<recvElemIter->getGlobalID();
            std::cout<<" face centroid = ["<<position[0]<<", "<<position[1]<<", "<<position[2]<<"],";
            std::cout<<" opp face has classification: "<<recvElemIter->getOppositeFace().getClassification()<<std::endl;
        }
    }
#endif

    foo(myMesh);
    
    MPI_Finalize();
    return 0;
}

void foo(const Geometry::MeshBase& M)
{

    //
    // Field Test
    //
    Geometry::Field<Geometry::MeshBase,Geometry::FaceBase,int> faceIntField(M);
    std::cout<<" field size= "<<faceIntField.size()<<std::endl;
    int j=0,myProc=0;
    
#ifdef USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD,&myProc);
#endif
    
    for(Geometry::MeshBase::FaceIterator fIt = M.faceBegin();fIt!=M.faceEnd(); ++fIt,j++)
    {
        faceIntField[*fIt] = j;
//         std::cout<<"proc "<<myProc<<", faceIntField["<<fIt->getLocalID()<<"]="<<faceIntField[*fIt]<<std::endl;
        
    }
    

}

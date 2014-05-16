// Algoritmi.cpp : main project file.

#include "stdafx.h"
#include <iostream>
#include <limits.h>
#include <string.h>
#include <queue>

using namespace std;
using namespace System;


void SwapValuePos( int *pValues, int iPos1, int iPos2 )
{
	int iTmp = pValues[iPos1];
	pValues[iPos1] = pValues[iPos2];
	pValues[iPos2] = iTmp;
}



int Partition( int *pC, int iFirst, int iLast )
{
	int x = pC[iLast];
	int i = iFirst-1;
	for( int j = iFirst; j <= iLast-1; j++ )
	{
		if( pC[j] <= x )
		{
			i++;
			SwapValuePos( pC, i, j );
		}
	}
	SwapValuePos( pC, i+1, iLast );
	return (i+1);
}


void QuickSort( int *pC, int iFirst, int iLast )
{
	if( iFirst < iLast )
	{
		int iMid = Partition( pC, iFirst, iLast );
		QuickSort( pC, iFirst, iMid-1 );
		QuickSort( pC, iMid+1, iLast );
	}
}


#define PARENT(i)		((i-1)/2)
#define CHILD_LEFT(i)	(i*2)+1
#define CHILD_RIGHT(i)	(i+1)*2

void MaxHeap( int *pC, int iSize, int iPos )
{
	int iLeft  = CHILD_LEFT(iPos);
	int iRight = CHILD_RIGHT(iPos);
	int iMax = iPos;

	if( iLeft <= iSize && pC[iLeft] > pC[iPos] )
	{
		iMax = iLeft;
	}
	if( iRight <= iSize && pC[iRight] > pC[iMax] )
	{
		iMax = iRight;
	}

	if( iMax != iPos )
	{
		int iVal = pC[iPos];
		pC[iPos] = pC[iMax];
		pC[iMax] = iVal;
		MaxHeap( pC, iSize, iMax );
	}
}

void HeapSort( int *pC, int iSize )
{
	int iLast = iSize - 1;
	int iRoot = iLast/2;

	for( int i = iRoot; i >= 0; i-- )
	{
		MaxHeap( pC, iLast, i );
	}

	int iUseful = iLast;
	while( iUseful > 0 )
	{
		SwapValuePos( pC, 0, iUseful );
		
		iUseful--;
		MaxHeap( pC, iUseful, 0 );
	}
}


void FindMaxCrossingSubArray( int *pA, int iLow, int mid, int iHi, int &iSubStart, int &iSubEnd, int &iSum )
{
	int iLeftSum = -32768;
	int sum = 0;

	int iMaxLeft, iMaxRight;

	for( int i = mid; i >= iLow; i-- )
	{
		sum += pA[i];
		if( sum > iLeftSum )
		{
			iLeftSum = sum;
			iMaxLeft = i;
		}
	}

	int iRightSum = -32768;
	sum = 0;

	for( int i = (mid+1); i <= iHi; i++ )
	{
		sum += pA[i];
		if( sum > iRightSum )
		{
			iRightSum = sum;
			iMaxRight = i;
		}
	}

	iSubStart = iMaxLeft;
	iSubEnd = iMaxRight;
	iSum = iLeftSum + iRightSum;
}


void FindMaximumSubArray( int *pA, int iLow, int iHi, int &iSubStart, int &iSubEnd, int &iSum )
{
	if( iHi == iLow )
	{
		iSubStart = iLow;
		iSubEnd	  = iHi;
		iSum      = pA[iLow];
	}
	else
	{
		int mid = ( iLow + iHi ) / 2;

		int iSubStartLeft, iSubEndLeft, iSumLeft, iSubStartRight, iSubEndRight, iSumRight;
		int iSubStartCross, iSubEndCross, iSumCross;
		FindMaximumSubArray( pA, iLow, mid, iSubStartLeft, iSubEndLeft, iSumLeft );
		FindMaximumSubArray( pA, mid+1, iHi, iSubStartRight, iSubEndRight, iSumRight );
		FindMaxCrossingSubArray( pA, iLow, mid, iHi, iSubStartCross, iSubEndCross, iSumCross );

		if( iSumLeft >= iSumRight && iSumLeft >= iSumCross )
		{
			iSubStart = iSubStartLeft;
			iSubEnd = iSubEndLeft;
			iSum = iSumLeft;
		}
		else if( iSumRight >= iSumLeft && iSumRight >= iSumCross )
		{
			iSubStart = iSubStartRight;
			iSubEnd = iSubEndRight;
			iSum = iSumRight;
		}
		else
		{
			iSubStart = iSubStartCross;
			iSubEnd = iSubEndCross;
			iSum = iSumCross;
		}
	}
}


//////////////////////////////////////////////////////////////////////////
//
// FORD FULKERSON
//
//////////////////////////////////////////////////////////////////////////
// Number of vertices in given graph
#define V 6
 
/* Returns true if there is a path from source 's' to sink 't' in
  residual graph. Also fills parent[] to store the path */
bool bfs(int rGraph[V][V], int s, int t, int parent[])
{
    // Create a visited array and mark all vertices as not visited
    bool visited[V];
    memset(visited, 0, sizeof(visited));
 
    // Create a queue, enqueue source vertex and mark source vertex
    // as visited
    queue <int> q;
    q.push(s);
    visited[s] = true;
    parent[s] = -1;
 
    // Standard BFS Loop
    while (!q.empty())
    {
        int u = q.front();
        q.pop();
 
        for (int v=0; v<V; v++)
        {
            if (visited[v]==false && rGraph[u][v] > 0)
            {
                q.push(v);
                parent[v] = u;
                visited[v] = true;
            }
        }
    }
 
    // If we reached sink in BFS starting from source, then return
    // true, else false
    return (visited[t] == true);
}
 
// Returns tne maximum flow from s to t in the given graph
int fordFulkerson(int graph[V][V], int s, int t)
{
    int u, v;
 
    // Create a residual graph and fill the residual graph with
    // given capacities in the original graph as residual capacities
    // in residual graph
    int rGraph[V][V]; // Residual graph where rGraph[i][j] indicates 
                     // residual capacity of edge from i to j (if there
                     // is an edge. If rGraph[i][j] is 0, then there is not)  
    for (u = 0; u < V; u++)
        for (v = 0; v < V; v++)
             rGraph[u][v] = graph[u][v];
 
    int parent[V];  // This array is filled by BFS and to store path
 
    int max_flow = 0;  // There is no flow initially
 
    // Augment the flow while tere is path from source to sink
    while (bfs(rGraph, s, t, parent))
    {
        // Find minimum residual capacity of the edhes along the
        // path filled by BFS. Or we can say find the maximum flow
        // through the path found.
        int path_flow = INT_MAX;
        for (v=t; v!=s; v=parent[v])
        {
            u = parent[v];
            path_flow = min(path_flow, rGraph[u][v]);
        }
 
        // update residual capacities of the edges and reverse edges
        // along the path
        for (v=t; v != s; v=parent[v])
        {
            u = parent[v];
            rGraph[u][v] -= path_flow;
            rGraph[v][u] += path_flow;
        }
 
        // Add path flow to overall flow
        max_flow += path_flow;
    }
 
    // Return the overall flow
    return max_flow;
}



//////////////////////////////////////////////////////////////////////////
//
// Cutting-rod
//
//////////////////////////////////////////////////////////////////////////
int Cut_Rod( int* Prices, int N )
{
	int Val = 0;

	if( N == 0 )
		return Val;

	int iCut;

	for( int c = 0; c < N; c++ )
	{
		iCut = c+1;

		Val = max( Val, Prices[c] +  Cut_Rod( Prices, N - iCut ) );
	}

	return Val;
}


//////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////
int main(array<System::String ^> ^args)
{
	/////////////////////////////////////////////////
	// Cutting-rod
	/////////////////////////////////////////////////
	int Prices[] = { 1, 5,8,9};
	int Value = Cut_Rod( Prices, 4 );



	int iCount = 0;

	/////////////////////////////////////////////////
	// Insertion sort
	/////////////////////////////////////////////////
	int A[] = { 6, 5,4,3,2,1};

	int key;

	for( int j =1; j < 6; j++ )
	{
		 iCount++;
		 int i = j - 1;

		 while( i >= 0 &&A[i]>A[i+1])
		 {
			 iCount++;
			 key = A[i+1];
			 A[i+1]=A[i];
			 A[i] = key;
			 i--;
		 }


	}


	/////////////////////////////////////////////////
	// 
	/////////////////////////////////////////////////
	int C[] = {13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };

	HeapSort( C, sizeof(C) / sizeof(int) );



	int D[] = {13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };

	QuickSort( D, 0, sizeof(D) / sizeof(int) -1 );



	/////////////////////////////////////////////////
	// divide et impera
	/////////////////////////////////////////////////
	int B[] = {13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };

	int iStart, iEnd, iSum;
	FindMaximumSubArray( B, 0, (sizeof(B)/sizeof(int))-1, iStart, iEnd, iSum ); 

	
	iCount = 0;

	int iNum = sizeof(B)/sizeof(int);
	iStart = iEnd = iSum = 0;
	int iTot = 0;
	for( int i = 0; i < iNum; i++ )
	{
		iCount++;

		iTot = 0;

		for( int j = i; j < iNum; j++ )
		{
			iCount++;

			iTot += B[j];
			if( iTot > iSum )
			{
				iSum = iTot;
				iStart = i;
				iEnd   = j;
			}
		}
	}


	//////////////////////////////////////////////////
	//
	// Ford Fulkerson
	//
	//////////////////////////////////////////////////
	// Let us create a graph shown in the above example
    int graph[V][V] = { {0, 16, 13, 0, 0, 0},
                        {0, 0, 10, 12, 0, 0},
                        {0, 4, 0, 0, 14, 0},
                        {0, 0, 9, 0, 0, 20},
                        {0, 0, 0, 7, 0, 4},
                        {0, 0, 0, 0, 0, 0}
                      };
 
    cout << "The maximum possible flow is " << fordFulkerson(graph, 0, 5);



    return 0;
}

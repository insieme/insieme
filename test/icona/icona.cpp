//============================================================================
// Name        : icona2.cpp
// Author      : Clemens Domanig
// Version     :
// Copyright   : 
// Description : icona
//============================================================================

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <cmath>
#include <omp.h>

#include "petsc.h"
#include "GL/glut.h"

#include "config.h"
#include "Node.h"
#include "Element.h"
#include "Material.h"
#include "Step.h"
#include "FETask.h"
#include "IFParser.h"

using namespace std;

static FETask femJob;
static double faktor=1;
static double camPos[3];
static double lookAt[3];
static double angle[3]={ 0};
static double minVV, maxVV;
static bool showNrs=false;

static const int nodeOrder[]={ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 6, 5, 2, 3, 7, 6, 3, 4, 8, 7, 4, 1, 5, 8};

extern int runCalc( PetscInt rank, PetscInt size, FETask &feJob);

string intToStr( int nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

void setMinMax() {
	minVV=femJob.getMinX();
	if( minVV>femJob.getMinY()) {
		minVV=femJob.getMinY();
	}
	if( minVV>femJob.getMinZ()) {
		minVV=femJob.getMinZ();
	}

	maxVV=femJob.getMaxX();
	if( maxVV<femJob.getMaxY()) {
		maxVV=femJob.getMaxY();
	}
	if( maxVV<femJob.getMaxZ()) {
		maxVV=femJob.getMaxZ();
	}

	double d=abs(maxVV-minVV);

	minVV=(minVV-d*0.1)*1.5;

	maxVV=(maxVV+d*0.1)*1.5;
}

void handleMouse( int button, int state, int x, int y) {
	bool redraw=false;

	if (state == GLUT_UP ) {
		if ( button==3) {
			for( int i=0; i<3; i++) {
				camPos[i]+=0.05*(lookAt[i]-camPos[i]);
			}
			redraw=true;
		}
		else if( button==4) {
			for( int i=0; i<3; i++) {
				camPos[i]-=0.05*(lookAt[i]-camPos[i]);
			}
			redraw=true;
		}
	}

	if( redraw) {
		glutPostRedisplay();
	}
}

void handleStandardKeys( unsigned char key, int x, int y) {
	bool redraw=false;

	if( key=='+') {
		faktor=faktor*1.5;
		redraw=true;
	}
	if( key=='-') {
		faktor=faktor/1.5;
		redraw=true;
	}
	if( key=='f') {
		faktor=1;
		redraw=true;
	}
	if( key=='r') {
		faktor=1;
		angle[0]=0;
		angle[1]=0;
		angle[2]=0;
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho( minVV, maxVV, minVV, maxVV, minVV, maxVV);
		redraw=true;
	}
	if( key=='q') {
		angle[0]+=ROT_ANGLE;
		redraw=true;
	}
	if( key=='a') {
		angle[0]-=ROT_ANGLE;
		redraw=true;
	}
	if( key=='w') {
		angle[1]+=ROT_ANGLE;
		redraw=true;
	}
	if( key=='s') {
		angle[1]-=ROT_ANGLE;
		redraw=true;
	}
	if( key=='e') {
		angle[2]+=ROT_ANGLE;
		redraw=true;
	}
	if( key=='d') {
		angle[2]-=ROT_ANGLE;
		redraw=true;
	}
	if( key=='n') {
		showNrs=!showNrs;
		redraw=true;
	}

	if( redraw) {
		glutPostRedisplay();
	}
}

void changeSize( int width, int height) {
	cout << "change" << endl;
    if( height==0) {		// Prevent a division by zero, when window's height=0
    	height=1;
    }

    float ratio = 1.0 * width / height;

    // Reset the coordinate system before modifying
    glMatrixMode( GL_PROJECTION);
    glLoadIdentity();

    // Set the viewport to be the entire window
    glViewport(0, 0, width, height);

    lookAt[0]=0.5*( femJob.getMaxX()+femJob.getMinX());
    lookAt[1]=0.5*( femJob.getMaxY()+femJob.getMinY());
    lookAt[2]=0.5*( femJob.getMaxZ()+femJob.getMinZ());
    double din=abs( femJob.getMaxY()-femJob.getMinY()) * 1.5;
    double dout=abs( femJob.getMaxX()-femJob.getMinX()) * 0.55;
    dout=dout / ( atan( 22.5/180*3.141592));

    camPos[0]=lookAt[0];
    camPos[1]=dout;//*femJob.getMaxY();
    camPos[2]=lookAt[2];

    // Set the correct perspective.
    gluPerspective( 45, ratio, dout, din);

    glMatrixMode( GL_MODELVIEW);
    glLoadIdentity();

    gluLookAt(	camPos[0], camPos[1], camPos[2],
				lookAt[0], lookAt[1], lookAt[2],
				1.0f, 0.0f, 0.0f);
}

void renderBitmapString( float x, float y,float z,void *font, const char *string) {
	const char *c;

	glRasterPos3f(x, y, z);

	for (c=string; *c != '\0'; c++) {
		glutBitmapCharacter(font, *c);
	}
}


void renderScene() {
	int i, j, localNr, globalNr;

    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	vector< Node>& nodes=femJob.getNodes();
	vector< Node>& nodesU=femJob.getNodesU();
	map< int, Element>& elements=femJob.getElements();
	map< int, Element>::iterator elementsIT;

    glRotatef( angle[0], 1, 0, 0);
    glRotatef( angle[1], 0, 1, 0);
    glRotatef( angle[2], 0, 0, 1);

	angle[0]=0;
	angle[1]=0;
	angle[2]=0;

	glBegin( GL_LINES );
		glColor3f(0.0f,1.0f,0.0f);
		glVertex3f( lookAt[0], lookAt[1], lookAt[2]);
		glVertex3f( lookAt[0]+20, lookAt[1], lookAt[2]);

		glColor3f(0.0f,0.0f,1.0f);
		glVertex3f( lookAt[0], lookAt[1], lookAt[2]);
		glVertex3f( lookAt[0], lookAt[1]+20, lookAt[2]);

		glColor3f(0.0f,1.0f,1.0f);
		glVertex3f( lookAt[0], lookAt[1], lookAt[2]);
		glVertex3f( lookAt[0], lookAt[1], lookAt[2]+20);
	glEnd();

	glPolygonMode( GL_FRONT_AND_BACK, GL_LINE);

	glColor3f( 1.0, 1.0, 1.0);

	for( elementsIT=elements.begin(); elementsIT!=elements.end(); elementsIT++) {
		Element& elem=(*elementsIT).second;

		for( i=0; i<6; i++) {
			glBegin(GL_POLYGON);

			for( j=0; j<4; j++) {
				localNr=nodeOrder[ 4*i + j] - 1;
				globalNr=elem.getGlobalNrOfNodeNr( localNr);

				glVertex3f( nodes[globalNr].getX(), nodes[globalNr].getY(), nodes[globalNr].getZ());
			}

			glEnd();
			//cout << endl;
		}
	}

	if( showNrs) {
		string nodeNrLabel;

		for( elementsIT=elements.begin(); elementsIT!=elements.end(); elementsIT++) {
			Element& elem=(*elementsIT).second;

			for( j=0; j<4; j++) {
				globalNr=elem.getGlobalNrOfNodeNr( j);

				nodeNrLabel=intToStr( nodes[globalNr].getOrigNodeNr());

				renderBitmapString( nodes[globalNr].getX(), nodes[globalNr].getY(), nodes[globalNr].getZ(), GLUT_BITMAP_TIMES_ROMAN_10, nodeNrLabel.c_str());
			}
			for( j=4; j<8; j++) {
				globalNr=elem.getGlobalNrOfNodeNr( j);

				nodeNrLabel=intToStr( nodes[globalNr].getOrigNodeNr());

				renderBitmapString( nodes[globalNr].getX(), nodes[globalNr].getY()+0.33, nodes[globalNr].getZ(), GLUT_BITMAP_TIMES_ROMAN_10, nodeNrLabel.c_str());
			}

		}
	}

	if( femJob.getSuccess()) {
		glColor3f( 1.0, 0.0, 0.0);

		for( elementsIT=elements.begin(); elementsIT!=elements.end(); elementsIT++) {
			Element& elem=(*elementsIT).second;

			for( i=0; i<6; i++) {
				glBegin(GL_POLYGON);

				for( j=0; j<4; j++) {
					localNr=nodeOrder[ 4*i + j] - 1;
					globalNr=elem.getGlobalNrOfNodeNr( localNr);

					glVertex3f( nodes[globalNr].getX()+faktor*nodesU[globalNr].getX(), nodes[globalNr].getY()+faktor*nodesU[globalNr].getY(), nodes[globalNr].getZ()+faktor*nodesU[globalNr].getZ());
				}

				glEnd();
			}
		}
	}

    glFlush();
}

//--------------------MAIN--------------------
int main(int argc, char *argv[]) {
	int errorNr=0, ompNrThr=0;
	int maxNrOmpThr;
	char inputfile[PETSC_MAX_PATH_LEN];
	char c[1];

	PetscErrorCode ierr;
	PetscMPIInt size, rank;
	PetscTruth ifFlag, viewFlag, calcFlag, ompFlag;

	PetscInitialize( &argc, &argv, 0, 0);
	MPI_Comm_rank( PETSC_COMM_WORLD, &rank);
	ierr=MPI_Comm_size( PETSC_COMM_WORLD, &size); CHKERRQ( ierr);
	ierr=MPI_Comm_rank( PETSC_COMM_WORLD, &rank); CHKERRQ( ierr);

	ierr=PetscOptionsGetString( PETSC_NULL, "-file", &inputfile[0], PETSC_MAX_PATH_LEN-1, &ifFlag); CHKERRQ(ierr);
	ierr=PetscOptionsGetString( PETSC_NULL, "-view", &c[0], 1, &viewFlag); CHKERRQ(ierr);
	ierr=PetscOptionsGetString( PETSC_NULL, "-calc", &c[0], 1, &calcFlag); CHKERRQ(ierr);

	ierr=PetscOptionsGetInt( PETSC_NULL, "-omp", &ompNrThr, &ompFlag); CHKERRQ(ierr);

	if( rank==0) {
		PetscPrintf( PETSC_COMM_WORLD, "\n");
		PetscPrintf( PETSC_COMM_WORLD, "icona - v%d.%d - %s\n", VERSION_ALL, VERSION_COUNT, VERSION_NAME);
		PetscPrintf( PETSC_COMM_WORLD, "parser   %1.1f\n", VERSION_PARSER);
		PetscPrintf( PETSC_COMM_WORLD, "calc     %1.1f\n", VERSION_CALC);
		PetscPrintf( PETSC_COMM_WORLD, "viewer   %1.1f\n\n", VERSION_VIEWER);

		//set openmp options
		#pragma omp parallel
		{
			maxNrOmpThr=omp_get_num_threads();
		}

		if( ompFlag) {
			if( (ompNrThr<1 ) && (ompNrThr>maxNrOmpThr )) {
				if( maxNrOmpThr<=2) {
					ompNrThr=maxNrOmpThr;
				}
				else {
					ompNrThr=(int) maxNrOmpThr / 2;
				}
			}
		}
		else {
			if( maxNrOmpThr<=2) {
				ompNrThr=maxNrOmpThr;
			}
			else {
				ompNrThr=(int) maxNrOmpThr / 2;
			}
		}

		femJob.setOmpNrThreads( ompNrThr);

		string InputFile( inputfile);
		IFParser jobParser( InputFile);

		PetscPrintf( PETSC_COMM_WORLD, "looking for input-file --> %s <--   ", jobParser.getInputFileName().c_str());

		if( jobParser.doesFileExist()) {
			PetscPrintf( PETSC_COMM_WORLD, "... done\ncreating log-file --> ");

			if( jobParser.createLogFile()) {
				PetscPrintf( PETSC_COMM_WORLD, "%s <--    ... done\n", jobParser.getLogFileName().c_str());

				femJob.setLogFileName( jobParser.getLogFileName());

				PetscPrintf( PETSC_COMM_WORLD, "generating temporary inputfile --> %s <--    ", jobParser.getTmpInputFileName().c_str());

				errorNr=jobParser.generateTmpInputFile();

				if( errorNr==0) {
					PetscPrintf( PETSC_COMM_WORLD, " ... done\n");

					PetscPrintf( PETSC_COMM_WORLD, "reading temporary inputfile --> %s <--    ", jobParser.getTmpInputFileName().c_str());

					errorNr=jobParser.readTmpInputFile( femJob);

					if( errorNr==0) {
						PetscPrintf( PETSC_COMM_WORLD, " ... done\n\n");
					}
				}
			}
			else {
				errorNr=ERR_NO_LOGFILE;
			}
		}
		else {
			errorNr=ERR_NO_INPUTFILE;
		}
	}

	PetscBarrier( (PetscObject) PETSC_NULL);

	if( errorNr==0) {
		if( calcFlag) {
			//start_calc
			errorNr=runCalc( rank, size, femJob);
		}

		if( viewFlag) {
			if( rank==0) {
				if( errorNr==0) {
					if( !femJob.getRenumbered()) {
						femJob.renumberNodeNrsElements();
					}

					glutInit( &argc, argv);

					glutInitDisplayMode(GLUT_DEPTH | GLUT_SINGLE | GLUT_RGBA);

					glutInitWindowPosition(100,100);
					glutInitWindowSize( WIN_WIDTH, WIN_HEIGHT);

					string winLabel( "icona - ");
					winLabel.append( femJob.getHeading());
					glutCreateWindow( winLabel.c_str());

					setMinMax();
					glMatrixMode(GL_PROJECTION);
					glLoadIdentity();

					double abc=(maxVV-minVV)/4;

					glOrtho( minVV, maxVV, minVV, maxVV, minVV, maxVV);
					glTranslatef( abc, abc, 0 );

					glutDisplayFunc( renderScene);
					//glutReshapeFunc( changeSize);
					glutKeyboardFunc( handleStandardKeys);
					glutMouseFunc( handleMouse);

					glutMainLoop();
				}
			}
		}
	}

	PetscBarrier( (PetscObject) PETSC_NULL);

	if( errorNr!=0) {
		PetscPrintf( PETSC_COMM_WORLD, "\n");

		switch( errorNr) {
			case ERR_NO_INPUTFILE:
				PetscPrintf( PETSC_COMM_WORLD, "error! - missing inputfile or does not exist\n");
				break;

			case ERR_TOO_FEW_ARGS:
				PetscPrintf( PETSC_COMM_WORLD, "error! - to few arguments\n");
				break;

			case ERR_IN_INPUTFILE:
				PetscPrintf( PETSC_COMM_WORLD, "\n\nerror! - error reading inputfile\n\t look at log-file for details\n");
				break;

			case ERR_NO_LOGFILE:
				PetscPrintf( PETSC_COMM_WORLD, "error! - cannot create logfile\n");
				break;
		}

		if( (errorNr>0) && (errorNr<3)) {
			PetscPrintf( PETSC_COMM_WORLD, "\nusage:\n");
			PetscPrintf( PETSC_COMM_WORLD, "\tcheck inputfile\t\ticona -file <inputfile>\n");
			PetscPrintf( PETSC_COMM_WORLD, "\trun job\t\t\ticona -file <inputfile> -calc -omp <nr>\n");
			PetscPrintf( PETSC_COMM_WORLD, "\tview inputfile\t\ticona -file <inputfile> -view\n");
			PetscPrintf( PETSC_COMM_WORLD, "\tview outfile\t\ticona -file <outfile> view\n");
			PetscPrintf( PETSC_COMM_WORLD, "\trun job + view\t\ticona -file <inputfile> -calc -view -omp <nr>\n");
		}

		PetscPrintf( PETSC_COMM_WORLD, "\n");
	}

	ierr=PetscFinalize(); CHKERRQ( ierr);

	return 0;
}

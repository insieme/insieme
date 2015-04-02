/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "MeasurementInfo.h"
#include <iostream>

std::list<MeasurementInfo> ParamUtility::infoList;

ParamUtility::ParamUtility() {

}

ParamUtility::~ParamUtility() {
}

void ParamUtility::initInfoList() {

	if (infoList.size() != params) {

		MeasurementInfo info[ParamUtility::params];


		info[0].param = Vrms;
		info[0].title = "vrms";
		info[0].description = "Root mean square Voltage";
		info[0].unit = "Volt (V)";
		info[0].formula = "Vrms = sqrt(1/T * integr_calc[0,T](pow(Vi,2))dt)";
		info[0].pmCommand = ":SEL:VLT\n";

		info[1].param = Arms;
		info[1].title = "arms";
		info[1].description = "Root mean square Current";
		info[1].unit = "Amp (A)";
		info[1].formula = "Arms = sqrt(1/T * integr_calc[0,T](pow(Ii,2))dt";
		info[1].pmCommand = ":SEL:AMP\n";

		info[2].param = F;
		info[2].title = "freq";
		info[2].description = "Frequenzy";
		info[2].unit = "Hertz (H)";
		info[2].formula = "";
		info[2].pmCommand = ":SEL:FRQ\n";

		info[3].param = W;
		info[3].title = "watt";
		info[3].description = "True Power";
		info[3].unit = "Watt (W)";
		info[3].formula = "W = 1/W integr_calc[0,T](pow(Vi * Ii)dt";
		info[3].pmCommand = ":SEL:WAT\n";

		info[4].param = PF;
		info[4].title = "pf";
		info[4].description = "Power Factor";
		info[4].unit = "";
		info[4].formula = "PF = [ Watt / (Vrms x Arms)]";
		info[4].pmCommand = ":SEL:PWF\n";

		info[5].param = VA;
		info[5].title = "va";
		info[5].description = "Apparent Power";
		info[5].unit = "Volt-Amps (VA)";
		info[5].formula = "VA = [Vrms x Arms]";
		info[5].pmCommand = ":SEL:VAS\n";

		info[6].param = VAr;
		info[6].title = "var";
		info[6].description = "Reactive Power";
		info[6].unit = "Volt-Amps Reactive (VAr)";
		info[6].formula = "VAr = sqrt(pow(VA,2) - pow(W,2))";
		info[6].pmCommand = ":SEL:VAR\n";

		info[7].param = Vcf;
		info[7].title = "vcf";
		info[7].description = "Voltage Crest Factor";
		info[7].unit = "";
		info[7].formula = "CF = PeakValue/RMSValue";
		info[7].pmCommand = ":SEL:VCF\n";

		info[8].param = Acf;
		info[8].title = "acf";
		info[8].description = "Current Crest Factor";
		info[8].unit = "";
		info[8].formula = "CF = PeakValue/RMSValue";
		info[8].pmCommand = ":SEL:ACF\n";

		info[9].param = Vthd;
		info[9].title = "vthd";
		info[9].description = "Voltage Total Harmonic Distortion";
		info[9].unit = "%";
		info[9].formula
				= "series = sqrt(pow(H0,2)+pow(H2,2)+pow(H3,2)+...)/REF or difference= sqrt(pow(Vrms,2) - pow(H1,2))/REF";
		info[9].pmCommand = ":SEL:VHM\n";

		info[10].param = Athd;
		info[10].title = "athd";
		info[10].description = "Current Total Harmonic Distortion";
		info[10].unit = "%";
		info[10].formula
				= "series = sqrt(pow(H0,2)+pow(H2,2)+pow(H3,2)+...)/REF or difference= sqrt(pow(Arms,2) - pow(H1,2))/REF";
		info[10].pmCommand = ":SEL:AHM\n";

		info[11].param = Z;
		info[11].title = "z";
		info[11].description = "Impedance";
		info[11].unit = "Ohm";
		info[11].formula = "Z = Vrms/Irms";
		info[11].pmCommand = ":SEL:IMP\n";

		info[12].param = Vdc;
		info[12].title = "vdc";
		info[12].description = "Direct Current voltage";
		info[12].unit = "Volt (V)";
		info[12].formula = "Vdc = 1/T integr_calc[0,T](v)dt";
		info[12].pmCommand = ":SEL:VDC\n";

		info[13].param = Adc;
		info[13].title = "adc";
		info[13].description = "DC Current";
		info[13].unit = "Amp (A)";
		info[13].formula = "Adc = 1/T integr_calc[0,T](i)dt";
		info[13].pmCommand = ":SEL:ADC\n";

		info[14].param = R;
		info[14].title = "r";
		info[14].description = "Resistance";
		info[14].unit = "Ohms";
		info[14].formula = "R = (Vf/Af) x cos(phase angle)";
		info[14].pmCommand = ":SEL:RES\n";

		info[15].param = X;
		info[15].title = "x";
		info[15].description = "Reactance";
		info[15].unit = "Ohms";
		info[15].formula = "X = (Vf/Af) x sin(phase angle)";
		info[15].pmCommand = ":SEL:REA\n";

		info[16].param = Vrng;
		info[16].title = "vrng";
		info[16].description = "Voltage harmonic n";
		info[16].unit = "Volt (V)";
		info[16].formula
				= "Mag = swrt(Vhn * pow(r,2) + Vhn * pow(q,2)); Phase = tan^(-1)((Vhn * q)/(Vhn * r))";
		info[16].pmCommand = ":SEL:VRNG\n";

		info[17].param = Arng;
		info[16].title = "arng";
		info[17].description = "Current harmonic n";
		info[17].unit = "Amp (A)";
		info[17].formula
				= "Mag = swrt(Ahn * pow(r,2) + Ahn * pow(q,2)); Phase = tan^(-1)((Ahn * q)/(Ahn * r))";
		info[16].pmCommand = ":SEL:ARNG\n";

		info[18].param = Vpk_pos;
		info[18].title = "vpk+";
		info[18].description = "(+)ve Peak Voltage";
		info[18].unit = "Volt (V)";
		info[18].formula = "";
		info[18].pmCommand = ":SEL:VPK+\n";

		info[19].param = Vpk_neg;
		info[19].title = "vpk-";
		info[19].description = "(-)ve Peak Voltage";
		info[19].unit = "Volt (V)";
		info[19].formula = "";
		info[19].pmCommand = ":SEL:VPK-\n";

		info[20].param = Apk_pos;
		info[20].title = "apk+";
		info[20].description = "(+)ve Peak Current";
		info[20].unit = "Amp (A)";
		info[20].formula = "";
		info[20].pmCommand = ":SEL:APK+\n";

		info[21].param = Apk_neg;
		info[21].title = "apk-";
		info[21].description = "(-)ve Peak Current";
		info[21].unit = "Amp (A)";
		info[21].formula = "";
		info[21].pmCommand = ":SEL:APK-\n";

		info[22].param = Whr;
		info[22].title = "Whr";
		info[22].description = "Enables Watt hours integration";
		info[22].unit = "W/hr";
		info[22].formula = "";
		info[22].pmCommand = ":SEL:WHR\n";


		for (unsigned int i = 0; i < params; i++) {
			ParamUtility::infoList.push_back(info[i]);
		}

	}

}

MeasurementInfo* ParamUtility::getMeasurementInfo(MEASUREMENT_PARAMS param) {

	initInfoList();

	std::list<MeasurementInfo>::iterator it = ParamUtility::infoList.begin();

	for (; it != ParamUtility::infoList.end(); it++) {

		if (it->param == param) {

			return &(*it);

		}

	}

	return NULL;

}
std::list<MeasurementInfo>* ParamUtility::getMeasurementParameterList() {

	initInfoList();

	return &(ParamUtility::infoList);

}


/*
 * SaveLoadQMHS4.h
 *
 *  Created on: 11.07.2011
 *      Author: manfred
 */

#ifndef SAVELOADQMHS4_H_
#define SAVELOADQMHS4_H_

#include "IcoVec.hpp"
#include "IcoMat.hpp"
#include <map>
typedef std::map<int, IcoVec> IcoVecMap;
typedef std::map<int, IcoMat> IcoMatMap;
class SaveLoadQMHS4 {
public:
	void setVecSig(IcoVecMap* sig_);
	void setVecStr(IcoVecMap* str_);
	void setVecDsig(IcoVecMap* Dsig_);
	void setVecDstr(IcoVecMap* Dstr_);
	void setInvF(IcoMatMap* invF_);
	void setInvFT(IcoMatMap* invFT_);
	void setG(IcoMatMap* G_);
	void setP11q(IcoMatMap* P11q_);
	void setP22(IcoMatMap* P22_);
	void setP21(IcoMatMap* P21_);
	void setT3(IcoMatMap* T3_);
	void setFe1q(IcoVecMap* fe1q_);
	void setFe2(IcoVecMap* fe2_);
	void setFs(IcoVecMap* fs_);
	int saveToFolder(const std::string pathToFolder,const std::string filenameAddition);

	SaveLoadQMHS4();
	virtual ~SaveLoadQMHS4();
private:
	IcoVecMap* sigG;
	IcoVecMap* strG;
	IcoVecMap* DsigG;
	IcoVecMap* DstrG;
	IcoMatMap* invFG;
	IcoMatMap* invFTG;
	IcoMatMap* GG;
	IcoMatMap* P11qG;
	IcoMatMap* P22G;
	IcoMatMap* P21G;
	IcoMatMap* T3E;
	IcoVecMap* fe1qG;
	IcoVecMap* fe2G;
	IcoVecMap* fsG;
	int saveVectorToFile(const std::string file, IcoVecMap* vec);
	int saveMatrixToFile(const std::string file, IcoMatMap* mat);
};

#endif /* SAVELOADQMHS4_H_ */

/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

////////////////////////////////////////////////////////////////////////////////////////////////
int value() {
	int a = 1;
	return a;
}

int& ref() {
	int a = 1;
	return a;
}

const int& constRef() {
	int a = 1;
	return a;
}

int* pointer() {
	int a = 1;
	return &a;
}
////////////////////////////////////////////////////////////////////////////////////////////////


class Obj {
	int a;

  public:
	Obj(int v = 3) : a(v) {}
	Obj value() {
		return *this;
	}
	Obj& ref() {
		return *this;
	}
	const Obj& constRef() {
		return *this;
	}
	Obj* pointer() {
		return this;
	}

	Obj value2() {
		Obj& o = *this;
		return o;
	}
	Obj& ref2() {
		Obj& o = *this;
		return o;
	}
	const Obj& constRef2() {
		Obj& o = *this;
		return o;
	}
	Obj* pointer2() {
		Obj& o = *this;
		return &o;
	}


	Obj value3() {
		const Obj& o = *this;
		return o;
	}
	const Obj& constRef3() {
		const Obj& o = *this;
		return o;
	}
};

//////////////////////////////////////////////////////////
Obj Objvalue() {
	Obj a;
	return a;
}
Obj& Objref() {
	Obj a;
	return a;
}
const Obj& ObjconstRef() {
	Obj a;
	return a;
}
Obj* Objpointer() {
	Obj a;
	return &a;
}

int main() {
	// primitives
	{
		value();
		ref();
		constRef();
		pointer();
	}

	// objects
	{
		Objvalue();
		Objref();
		ObjconstRef();
		Objpointer();
	}

	// members
	{
		Obj o;
		o.value();
		o.ref();
		o.constRef();
		o.pointer();
	}

	// members 2
	{
		Obj o;
		o.value2();
		o.ref2();
		o.constRef2();
		o.pointer2();
	}

	// members 3
	{
		Obj o;
		o.value3();
		o.constRef3();
	}
	return 0;
}

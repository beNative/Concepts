#include <vcl.h>
#include <windows.h>

#pragma hdrstop

#include <tchar.h>

#include "Impl.h"

//#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	CppUnit::Register(*InitApp());
	RunApp();
	return 0;
}

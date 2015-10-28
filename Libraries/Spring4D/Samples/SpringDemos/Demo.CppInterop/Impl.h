
#include <stdio.h>

#include "DelphiUnit.hpp"
#include "Services.hpp"

namespace CppUnit
{
	class TCppImpl1 : public TInterfacedObject, public IService1
	{
	public:
		void __fastcall Foo()
		{
			printf("TCppImpl1.Foo\n");
		}

	public:
		INTFOBJECT_IMPL_IUNKNOWN(TInterfacedObject);
	};

	#pragma push explicit_rtti
	#pragma explicit_rtti methods (__published, public) properties (__published, public) fields(__published, public, protected, private)
	class TCppImpl1WithDelphiDep : public TInterfacedObject, public IService1
	{
	private:
		//could work if Spring is modified, type name is correct byt TypeInfo
		//is different but looks the same also TypeData is different.
		//Maybe using property would work fine.
		//DelphiInterface<IService2> m_dep;
		IService2* m_dep;
	public:
		void __fastcall Foo()
		{
			printf("TCppImpl1WithDelphiDep.Foo\n");
			m_dep->Bar();
		}

		__fastcall ~TCppImpl1WithDelphiDep()
		{
			//We need to manually release the object
			m_dep->Release();
		}
	public:
		INTFOBJECT_IMPL_IUNKNOWN(TInterfacedObject);
	};
	#pragma pop explicit_rtti

	void Register(IContainer& container)
	{
#ifdef _DEBUG
		bool i = false;
		if (i)
		{
			//Check abstract implementations
			new TCppImpl1();
		}
#endif

		container.RegisterType(__delphirtti(TCppImpl1))
			->AsDefault()
			->AsSingleton();

		container.RegisterType(__delphirtti(TCppImpl1WithDelphiDep))
			->Implements(__delphirtti(IService1), "cppdep")
			->InjectField("m_dep");
	}
}
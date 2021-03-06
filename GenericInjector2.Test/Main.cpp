#include <InjectedFunction.h>
#include <cassert>

int InjectedFunction(int a, short& b, int c)
{
	return a + b + c;
}

int main()
{
	LPBYTE stackPointer;
	DWORD ecxValue, edxValue;

	short tmpShort = 1;

	__asm
	{
		push 0
		lea eax, dword ptr[tmpShort]
		push eax
		push 2

		mov stackPointer, esp
		mov ecxValue, ecx
		mov edxValue, edx
	}

	GenericInjector2::InjectedFunction<decltype(&InjectedFunction)> injectedFunction{ stackPointer, ecxValue, edxValue };
	const auto ret = injectedFunction.Invoke([](int a, short& b, int c)
	{
		const auto result = a - b - c;
		b = 5;
		return result;
	});
	assert(ret == 1);
	assert(tmpShort == 5);

	__asm
	{
		add esp, 12
	}
}

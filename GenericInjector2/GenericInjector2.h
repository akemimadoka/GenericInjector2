#pragma once

namespace GenericInjector2
{
	class Injector
	{
	public:
		Injector();
		virtual ~Injector();

	private:
		virtual void OnLoad() = 0;
		virtual void OnUnload() = 0;
	};
}

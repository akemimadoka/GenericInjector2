#pragma once

#include <Windows.h>

#include "FunctionInfo.h"

namespace GenericInjector2
{
	namespace Detail
	{
		// 在 x86/msvc 环境下假设参数对齐到 DWORD
		static constexpr std::size_t DefaultAlignment = sizeof(DWORD);

		template <typename T>
		struct ArgumentMap
		{
			using Type = std::decay_t<T>;
		};

		// 在 msvc abi 中引用与指针的表示相同
		template <typename T>
		struct ArgumentMap<T&>
		{
			using Type = typename ArgumentMap<T>::Type*;
		};

		template <typename T>
		struct ArgumentMap<T&&>
		{
			using Type = typename ArgumentMap<T>::Type*;
		};

		template <typename... T>
		struct OffsetCalculator
		{
			static constexpr std::size_t Value = Traits::Sum(Traits::AlignTo(sizeof(typename ArgumentMap<T>::Type), DefaultAlignment)...);
		};

		class ArgumentStorageBase
		{
		public:
			constexpr ArgumentStorageBase(LPBYTE stackPointer, DWORD& ecx, DWORD& edx)
				: m_StackPointer{ stackPointer }, m_Ecx{ ecx }, m_Edx{ edx }
			{
			}

		protected:
			LPBYTE m_StackPointer;
			DWORD &m_Ecx, &m_Edx;
		};

		template <typename FunctionInfo, Traits::CallingConventionType CallingConvention>
		struct GetArgumentImpl;

		template <typename FunctionInfo>
		struct GetArgumentImpl<FunctionInfo, Traits::CallingConventionType::Cdecl>
			: ArgumentStorageBase
		{
			using ArgumentStorageBase::ArgumentStorageBase;

			template <std::size_t I>
			constexpr decltype(auto) Get() const noexcept
			{
				using ResultType = typename Traits::GetSequenceElem<typename FunctionInfo::ArgTuple, I>::Type;
				decltype(auto) result = *reinterpret_cast<typename ArgumentMap<ResultType>::Type*>(
					m_StackPointer + Traits::ApplyTo<typename Traits::PopBack<
					typename FunctionInfo::ArgTuple, Traits::GetSequenceSize<typename FunctionInfo::ArgTuple>::value - I>::Type, OffsetCalculator>::Value);

				// 引用被解释为指针，这里还原
				if constexpr (std::is_reference_v<ResultType>)
				{
					return std::forward<ResultType>(*result);
				}
				else
				{
					return std::forward<ResultType>(result);
				}
			}
		};

		// Stdcall 和 Cdecl 对参数传递的规则相同
		template <typename FunctionInfo>
		struct GetArgumentImpl<FunctionInfo, Traits::CallingConventionType::Stdcall>
			: ArgumentStorageBase
		{
			using ArgumentStorageBase::ArgumentStorageBase;

			template <std::size_t I>
			constexpr decltype(auto) Get() const noexcept
			{
				using ResultType = typename Traits::GetSequenceElem<typename FunctionInfo::ArgTuple, I>::Type;
				decltype(auto) result = *reinterpret_cast<typename ArgumentMap<ResultType>::Type*>(
					m_StackPointer + Traits::ApplyTo<typename Traits::PopBack<
					typename FunctionInfo::ArgTuple, Traits::GetSequenceSize<typename FunctionInfo::ArgTuple>::value - I>::Type, OffsetCalculator>::Value);

				// 引用被解释为指针，这里还原
				if constexpr (std::is_reference_v<ResultType>)
				{
					return std::forward<ResultType>(*result);
				}
				else
				{
					return std::forward<ResultType>(result);
				}
			}
		};
	}

	template <typename Func>
	class ArgumentStorage
		: public Detail::GetArgumentImpl<Traits::FunctionInfo<Func>, Traits::FunctionInfo<Func>::CallingConvention>
	{
		using Base = Detail::GetArgumentImpl<Traits::FunctionInfo<Func>, Traits::FunctionInfo<Func>::CallingConvention>;
	public:
		using Base::Base;
	};

	template <typename Func>
	struct InjectedFunction
		: ArgumentStorage<Func>
	{
		using Base = ArgumentStorage<Func>;
		using Base::Base;

		using FunctionInfo = Traits::FunctionInfo<Func>;

		template <typename F>
		constexpr decltype(auto) Invoke(F&& func)
		{
			return invokeImpl(std::forward<F>(func), std::make_index_sequence<Traits::GetSequenceSize<typename FunctionInfo::ArgTuple>::value>{});
		}

	private:
		template <typename F, std::size_t... I>
		constexpr decltype(auto) invokeImpl(F&& func, std::index_sequence<I...>)
		{
			return std::forward<F>(func)(this->template Get<I>()...);
		}
	};
}

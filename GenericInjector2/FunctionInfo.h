#pragma once
#include <type_traits>
#include <tuple>

namespace GenericInjector2
{
	namespace Traits
	{
		// 工具函数

		constexpr std::size_t AlignTo(std::size_t size, std::size_t alignment)
		{
			return (size + alignment - 1) & ~(alignment - 1);
		}

		template <typename... Args>
		constexpr decltype(auto) Sum(Args&&... args)
		{
			return (0 + ... + std::forward<Args>(args));
		}

		// 通用 Trait

		template <typename T>
		struct Identity
		{
			using Type = T;
		};

		template <typename From, template <typename...> class To>
		struct ApplyToTrait;

		template <template <typename...> class FromTemp, template <typename...> class To, typename... Args>
		struct ApplyToTrait<FromTemp<Args...>, To>
		{
			using Type = To<Args...>;
		};

		template <typename From, template <typename...> class To>
		using ApplyTo = typename ApplyToTrait<From, To>::Type;

		template <typename T1, typename T2>
		struct PassReference;

		template <typename T1, typename T2>
		struct PassReference<T1&, T2>
		{
			using Type = T2&;
		};

		template <typename T1, typename T2>
		struct PassReference<T1&&, T2>
		{
			using Type = T2&&;
		};

		// 序列操作相关 Trait

		template <typename Seq>
		struct GetSequenceType;

		template <template <typename T, T...> class SeqTemp, typename T, T... Elem>
		struct GetSequenceType<SeqTemp<T, Elem...>>
		{
			using Type = T;
		};

		template <typename Seq>
		struct GetSequenceSize;

		template <template <typename T, T...> class SeqTemp, typename T, T... Elem>
		struct GetSequenceSize<SeqTemp<T, Elem...>>
			: std::integral_constant<std::size_t, sizeof...(Elem)>
		{
		};

		template <template <typename... T> class TypeSeqTemp, typename... T>
		struct GetSequenceSize<TypeSeqTemp<T...>>
			: std::integral_constant<std::size_t, sizeof...(T)>
		{
		};

		template <typename Seq, template <typename T, T> class ElemOp = std::integral_constant, typename IndexSeq = std::make_index_sequence<GetSequenceSize<Seq>::value>>
		struct SequenceManipulator;

		template <template <typename T, T...> class SeqTemp, typename T, T... Elem, template <typename TOp, TOp> class ElemOp, std::size_t... Index>
		struct SequenceManipulator<SeqTemp<T, Elem...>, ElemOp, std::index_sequence<Index...>>
		{
			static constexpr T InternalArray[] = { ElemOp<T, Elem>::value... };
			using Type = SeqTemp<T, InternalArray[Index]...>;
			using OriginSequence = SeqTemp<T, Elem...>;
		};

		// 空数组是禁止的，因此特化一下
		template <template <typename T, T...> class SeqTemp, typename T, template <typename TOp, TOp> class ElemOp>
		struct SequenceManipulator<SeqTemp<T>, ElemOp, std::index_sequence<>>
		{
			using Type = SeqTemp<T>;
			using OriginSequence = SeqTemp<T>;
		};

		template <typename Seq, std::size_t I>
		struct GetSequenceElem;

		template <template <typename...> class SeqTemp, std::size_t I, typename TFirst, typename... TRest>
		struct GetSequenceElem<SeqTemp<TFirst, TRest...>, I>
			: GetSequenceElem<SeqTemp<TRest...>, I - 1>
		{
		};

		template <template <typename...> class SeqTemp, typename TFirst, typename... TRest>
		struct GetSequenceElem<SeqTemp<TFirst, TRest...>, 0>
		{
			using Type = TFirst;
		};

		template <template <typename T, T...> class SeqTemp, std::size_t I, typename T, T... Elem>
		struct GetSequenceElem<SeqTemp<T, Elem...>, I>
		{
			static constexpr T InternalArray[] = { Elem... };
			static constexpr T Value = InternalArray[I];
		};

		template <typename TypeSeq, template <typename> class ElemOp = Identity, typename IndexSeq = std::make_index_sequence<GetSequenceSize<TypeSeq>::value>>
		struct TypeSequenceManipulator;

		template <template <typename...> class TypeSeqTemp, typename... T, template <typename> class ElemOp, std::size_t... Index>
		struct TypeSequenceManipulator<TypeSeqTemp<T...>, ElemOp, std::index_sequence<Index...>>
		{
			using OriginSequence = TypeSeqTemp<T...>;
			using Type = TypeSeqTemp<typename ElemOp<typename GetSequenceElem<OriginSequence, Index>::Type>::Type...>;
		};

		enum class SequenceClass
		{
			Invalid,
			ElemSequence,	// 类似 std::integer_sequence
			TypeSequence	// 类似 std::tuple
		};

		template <typename Seq>
		struct GetSequenceClass
			: std::integral_constant<SequenceClass, SequenceClass::Invalid>
		{
		};

		template <template <typename T, T...> class SeqTemp, typename T, T... Elem>
		struct GetSequenceClass<SeqTemp<T, Elem...>>
			: std::integral_constant<SequenceClass, SequenceClass::ElemSequence>
		{
		};

		template <template <typename...> class SeqTemp, typename... T>
		struct GetSequenceClass<SeqTemp<T...>>
			: std::integral_constant<SequenceClass, SequenceClass::TypeSequence>
		{
		};

		namespace Detail
		{
			template <typename Seq, SequenceClass Class, std::size_t N>
			struct PopFrontImpl;

			template <typename T, T Amount>
			struct AddOp
			{
				template <typename TValue, TValue Value>
				struct Op
					: std::integral_constant<decltype(Value + Amount), Value + Amount>
				{
				};
			};

			template <typename Seq, std::size_t N>
			struct PopFrontImpl<Seq, SequenceClass::ElemSequence, N>
				: SequenceManipulator<Seq, std::integral_constant, typename SequenceManipulator<std::make_index_sequence<GetSequenceSize<Seq>::value - N>, AddOp<std::size_t, N>::template Op>::Type>
			{
			};

			template <typename Seq, std::size_t N>
			struct PopFrontImpl<Seq, SequenceClass::TypeSequence, N>
				: TypeSequenceManipulator<Seq, Identity, typename SequenceManipulator<std::make_index_sequence<GetSequenceSize<Seq>::value - N>, AddOp<std::size_t, N>::template Op>::Type>
			{
			};

			template <typename Seq, SequenceClass Class, std::size_t N>
			struct PopBackImpl;

			template <typename Seq, std::size_t N>
			struct PopBackImpl<Seq, SequenceClass::ElemSequence, N>
				: SequenceManipulator<Seq, std::integral_constant, std::make_index_sequence<GetSequenceSize<Seq>::value - N>>
			{
			};

			template <typename Seq, std::size_t N>
			struct PopBackImpl<Seq, SequenceClass::TypeSequence, N>
				: TypeSequenceManipulator<Seq, Identity, std::make_index_sequence<GetSequenceSize<Seq>::value - N>>
			{
			};
		}

		template <typename Seq, std::size_t N = 1>
		struct PopFront
			: Detail::PopFrontImpl<Seq, GetSequenceClass<Seq>::value, N>
		{
		};

		template <typename Seq, std::size_t N = 1>
		struct PopBack
			: Detail::PopBackImpl<Seq, GetSequenceClass<Seq>::value, N>
		{
		};

		template <typename Seq, std::size_t Begin, std::size_t Count>
		struct SubSequence
			: PopBack<typename PopFront<Seq, Begin>::Type, GetSequenceSize<Seq>::value - Begin - Count>
		{
		};

		template <typename Seq1, typename Seq2>
		struct ConcatSequence;

		template <template <typename T, T...> class SeqTemp, typename T, T... Elem1, T... Elem2>
		struct ConcatSequence<SeqTemp<T, Elem1...>, SeqTemp<T, Elem2...>>
		{
			using Type = SeqTemp<T, Elem1..., Elem2...>;
		};

		template <template <typename...> class SeqTemp, typename... Elem1, typename... Elem2>
		struct ConcatSequence<SeqTemp<Elem1...>, SeqTemp<Elem2...>>
		{
			using Type = SeqTemp<Elem1..., Elem2...>;
		};

		template <typename Seq, std::size_t Begin, std::size_t Count>
		struct EraseSequence
			: ConcatSequence<typename PopBack<Seq, GetSequenceSize<Seq>::value - Begin>::Type, typename PopFront<Seq, Begin + Count>::Type>
		{
		};

		// 函数信息 Trait

		enum class CallingConventionType
		{
			// x86
			Cdecl,
			Stdcall,
			Thiscall,
			Fastcall,

			// x86_64
			X64Call = Fastcall
		};

		template <typename Func>
		struct FunctionInfo;

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(Args...)>
		{
			using ReturnType = Ret;
			using ArgTuple = std::tuple<Args...>;
			static constexpr bool IsVarArg = false;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(Args..., ...)>
		{
			using ReturnType = Ret;
			using ArgTuple = std::tuple<Args...>;
			static constexpr bool IsVarArg = true;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl *)(Args...)>
			: FunctionInfo<Ret(Args...)>
		{
			static constexpr CallingConventionType CallingConvention = CallingConventionType::Cdecl;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl *)(Args..., ...)>
			: FunctionInfo<Ret(Args..., ...)>
		{
			static constexpr CallingConventionType CallingConvention = CallingConventionType::Cdecl;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args...)>
			: FunctionInfo<Ret(__cdecl *)(Args...)>
		{
			using ClassType = T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args..., ...)>
			: FunctionInfo<Ret(__cdecl *)(Args..., ...)>
		{
			using ClassType = T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args...) const>
			: FunctionInfo<Ret(__cdecl *)(Args...)>
		{
			using ClassType = const T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args..., ...) const>
			: FunctionInfo<Ret(__cdecl *)(Args..., ...)>
		{
			using ClassType = const T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args...) volatile>
			: FunctionInfo<Ret(__cdecl *)(Args...)>
		{
			using ClassType = volatile T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args..., ...) volatile>
			: FunctionInfo<Ret(__cdecl *)(Args..., ...)>
		{
			using ClassType = volatile T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args...) const volatile>
			: FunctionInfo<Ret(__cdecl *)(Args...)>
		{
			using ClassType = const volatile T;
		};

		template <typename T, typename Ret, typename... Args>
		struct FunctionInfo<Ret(__cdecl T::*)(Args..., ...) const volatile>
			: FunctionInfo<Ret(__cdecl *)(Args..., ...)>
		{
			using ClassType = const volatile T;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(__stdcall *)(Args...)>
			: FunctionInfo<Ret(Args...)>
		{
			static constexpr CallingConventionType CallingConvention = CallingConventionType::Stdcall;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(__thiscall *)(Args...)>
			: FunctionInfo<Ret(Args...)>
		{
			static constexpr CallingConventionType CallingConvention = CallingConventionType::Thiscall;
		};

		template <typename Ret, typename... Args>
		struct FunctionInfo<Ret(__fastcall *)(Args...)>
			: FunctionInfo<Ret(Args...)>
		{
			static constexpr CallingConventionType CallingConvention = CallingConventionType::Fastcall;
		};

		template <bool VarArg, CallingConventionType CallingConvention, typename Ret, typename... Args>
		struct MakeFunctionPointer;

		template <typename Ret, typename... Args>
		struct MakeFunctionPointer<false, CallingConventionType::Cdecl, Ret, Args...>
		{
			using Type = Ret(__cdecl *)(Args...);
		};

		template <typename Ret, typename... Args>
		struct MakeFunctionPointer<true, CallingConventionType::Cdecl, Ret, Args...>
		{
			using Type = Ret(__cdecl *)(Args..., ...);
		};

		template <typename Ret, typename... Args>
		struct MakeFunctionPointer<false, CallingConventionType::Stdcall, Ret, Args...>
		{
			using Type = Ret(__stdcall *)(Args...);
		};

		template <typename Ret, typename... Args>
		struct MakeFunctionPointer<false, CallingConventionType::Thiscall, Ret, Args...>
		{
			using Type = Ret(__thiscall *)(Args...);
		};

		template <typename Ret, typename... Args>
		struct MakeFunctionPointer<false, CallingConventionType::Fastcall, Ret, Args...>
		{
			using Type = Ret(__fastcall *)(Args...);
		};
	}
}

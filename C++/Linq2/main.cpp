#include "stdafx.h"

//-------------------------------------------------------------------------
template<typename T>
struct IEnumerator {
	virtual ~IEnumerator() {}
	virtual bool moveNext() = 0;
	virtual T current() = 0;
};
template<typename T>
using IEnumeratorPtr = shared_ptr<IEnumerator<T>>;

template<typename T>
struct IEnumerable {
	typedef T Type;
	virtual ~IEnumerable(){}
	virtual IEnumeratorPtr<T> getEnumerator() = 0;
};
template<typename T>
using IEnumerablePtr = shared_ptr<IEnumerable<T>>;
//-------------------------------------------------------------------------
template<typename CT>
struct ContainerTraits {
	typedef typename CT::value_type ValueT;
	typedef typename CT::const_iterator ConstIterT;
};
template<typename T, int n>
struct ContainerTraits<T[n]> {
	typedef typename remove_cv<T>::type ValueT;
	typedef T const* ConstIterT;
};

template<typename CT>
class STLEnumerable : public IEnumerable<typename ContainerTraits<CT>::ValueT> {
private:
	typedef typename ContainerTraits<CT>::ValueT ValueT;
	typedef typename ContainerTraits<CT>::ConstIterT ConstIterT;

	class Enumerator : public IEnumerator<ValueT> {
	public:
		explicit Enumerator(CT const &container) : mCurr(begin(container)), mEnd(end(container)) {
		}
		bool moveNext() override final {
			if (mCurr != mEnd) {
				mValue = *mCurr;
				++mCurr;
				return true;
			}
			return false;
		}
		ValueT current() override final {
			return mValue;
		}
	private:
		typename ConstIterT mCurr, mEnd;
		typename ValueT mValue;
	};

public:
	explicit STLEnumerable(CT const &container) : mContainer(container) {
	}

	IEnumeratorPtr<ValueT> getEnumerator() override {
		return make_shared<Enumerator>(mContainer);
	}

private:
	CT const &mContainer;
};

template<typename CT>
class STLContainerEnumerable : public STLEnumerable<CT> {
public:
	explicit STLContainerEnumerable(CT const &container) : mContainer(container), STLEnumerable<CT>(mContainer) {
	}
	explicit STLContainerEnumerable(CT &&container) : mContainer(move(container)), STLEnumerable<CT>(mContainer) {
	}

private:
	CT mContainer;
};

template<typename T, typename FuncT>
class FilterEnumerable : public IEnumerable<T> {
private:
	class Enumerator : public IEnumerator<T> {
	public:
		Enumerator(IEnumeratorPtr<T> prevEnumerator, FuncT const &f) : mPreEnumerator(prevEnumerator), mF(f) {
		}
		bool moveNext() override final {
			while (mPreEnumerator->moveNext()) {
				if (mF(mPreEnumerator->current())) return true;
			}
			return false;
		}
		T current() override final {
			return mPreEnumerator->current();
		}
	private:
		IEnumeratorPtr<T> mPreEnumerator;
		FuncT mF;
	};

public:

	FilterEnumerable(IEnumerablePtr<T> prevEnumereable, FuncT const &f) : mPrevEnumereable(prevEnumereable), mF(f) {
	}

	IEnumeratorPtr<T> getEnumerator() override final {
		return make_shared<Enumerator>(mPrevEnumereable->getEnumerator(), mF);
	}

private:
	IEnumerablePtr<T> mPrevEnumereable;
	FuncT mF;
};

template<typename T, typename T2, typename FuncT>
class MapEnumerable : public IEnumerable<T2> {
private:
	class Enumerator : public IEnumerator<T2> {
	public:
		Enumerator(IEnumeratorPtr<T> prevEnumerator, FuncT const &f) : mPreEnumerator(prevEnumerator), mF(f) {
		}
		bool moveNext() override final {
			return mPreEnumerator->moveNext();
		}
		T2 current() override final {
			return mF(mPreEnumerator->current());
		}
	private:
		IEnumeratorPtr<T> mPreEnumerator;
		FuncT mF;
	};

public:

	MapEnumerable(IEnumerablePtr<T> prevEnumereable, FuncT const &f) : mPrevEnumereable(prevEnumereable), mF(f) {
	}

	IEnumeratorPtr<T2> getEnumerator() override final {
		return make_shared<Enumerator>(mPrevEnumereable->getEnumerator(), mF);
	}

private:
	IEnumerablePtr<T> mPrevEnumereable;
	FuncT mF;
};

template<bool take, typename T, typename FuncT>
class TakeEnumerable : public IEnumerable<T> {
private:
	class Enumerator : public IEnumerator<T> {
	public:
		Enumerator(IEnumeratorPtr<T> prevEnumerator, FuncT const &f) : mPreEnumerator(prevEnumerator), mF(f), mFailed(false) {
		}
		bool moveNext() override final {
			if (take) {
				if (mFailed) return false;
				if (!mPreEnumerator->moveNext()) return false;
				mFailed = !mF(mPreEnumerator->current());
				return !mFailed;
			} else {
				if (mFailed) return mPreEnumerator->moveNext();
				for (;;) {
					if (!mPreEnumerator->moveNext()) return false;
					if (!mF(mPreEnumerator->current())) break;
				}
				return mFailed = true;
			}
		}
		T current() override final {
			return mPreEnumerator->current();
		}
	private:
		IEnumeratorPtr<T> mPreEnumerator;
		FuncT mF;
		bool mFailed;
	};

public:

	TakeEnumerable(IEnumerablePtr<T> prevEnumereable, FuncT const &f) : mPrevEnumereable(prevEnumereable), mF(f) {
	}

	IEnumeratorPtr<T> getEnumerator() override final {
		return make_shared<Enumerator>(mPrevEnumereable->getEnumerator(), mF);
	}

private:
	IEnumerablePtr<T> mPrevEnumereable;
	FuncT mF;
};

template<bool positive>
class RangeEnumerable : public IEnumerable<int> {
private:
	class Enumerator : public IEnumerator<int> {
	public:
		Enumerator(int curr, int end, int step) : mCurr(curr), mEnd(end), mStep(step) {
		}
		bool moveNext() override final {
			if ((positive && mCurr >= mEnd) || (!positive && mCurr <= mEnd)) return false;
			mCurr += mStep;
			return true;
		}
		int current() override final {
			return mCurr - mStep;
		}
	private:
		int mCurr, mEnd, mStep;
	};

public:

	RangeEnumerable(int begin, int end, int step) : mBegin(begin), mEnd(end), mStep(step) {
	}

	IEnumeratorPtr<int> getEnumerator() override final {
		return make_shared<Enumerator>(mBegin, mEnd, mStep);
	}

private:
	int mBegin, mEnd, mStep;
};
//-------------------------------------------------------------------------
template<typename T>
class LinqObject {
public:
	explicit LinqObject(IEnumerablePtr<T> enumerable) : mEnumerable(enumerable){}

	template<typename FuncT>
	LinqObject filter(FuncT const &f) {
		return LinqObject(make_shared<FilterEnumerable<T, FuncT>>(mEnumerable, f));
	}

	template<typename FuncT>
	LinqObject<typename result_of<FuncT(T)>::type> map(FuncT const &f) {
		typedef typename result_of<FuncT(T)>::type T2;
		return LinqObject<T2>(make_shared<MapEnumerable<T, T2, FuncT>>(mEnumerable, f));
	}

	template<typename T2, typename FuncT>
	T2 reduce(T2 init, FuncT const &f) {
		for (auto it = mEnumerable->getEnumerator(); it->moveNext(); init = f(init, it->current()));
		return init;
	}

	T sum() {
		return reduce(T(), [](T a, T b){return a + b; });
	}

	bool empty() {
		return !mEnumerable->getEnumerator()->moveNext();
	}

	int size() {
		int n = 0;
		for (auto it = mEnumerable->getEnumerator(); it->moveNext(); ++n);
		return n;
	}

	template<typename FuncT>
	void foreach(FuncT const &f) {
		for (auto it = mEnumerable->getEnumerator(); it->moveNext(); f(it->current()));
	}

	bool equals(LinqObject &o) {
		auto it1 = mEnumerable->getEnumerator(), it2 = o.mEnumerable->getEnumerator();
		while (it1->moveNext()) {
			if (!it2->moveNext()) return false;
			if (it1->current() != it2->current()) return false;
		}
		return !it2->moveNext();
	}

	LinqObject reverse() {
		auto v = toVector();
		std::reverse(v.begin(), v.end());
		return LinqObject(make_shared<STLContainerEnumerable<vector<T>>>(move(v)));
	}

	template<typename FuncT>
	T find(FuncT const &f) {
		for (auto it = mEnumerable->getEnumerator(); it->moveNext();) {
			if (f(it->current())) return it->current();
		}
		return T();
	}

	template<typename FuncT>
	bool all(FuncT const &f) {
		for (auto it = mEnumerable->getEnumerator(); it->moveNext();) {
			if (!f(it->current())) return false;
		}
		return true;
	}

	template<typename FuncT>
	LinqObject sorted(FuncT const &f) {
		auto v = toVector();
		sort(v.begin(), v.end(), f);
		return LinqObject(make_shared<STLContainerEnumerable<vector<T>>>(move(v)));
	}

	template<typename FuncT>
	bool any(FuncT const & f) {
		for (auto it = mEnumerable->getEnumerator(); it->moveNext();) {
			if (f(it->current())) return true;
		}
		return false;
	}

	LinqObject take(int n) {
		int i = 0;
		return takeWhile([=](T const &)mutable{ return i++ < n; });
	}

	template<typename FuncT>
	LinqObject takeWhile(FuncT const &f) {
		return LinqObject(make_shared<TakeEnumerable<true, T, FuncT>>(mEnumerable, f));
	}

	LinqObject drop(int n) {
		int i = 0;
		return dropWhile([=](T const &)mutable{ return i++ < n; });
	}

	template<typename FuncT>
	LinqObject dropWhile(FuncT const &f) {
		return LinqObject(make_shared<TakeEnumerable<false, T, FuncT>>(mEnumerable, f));
	}

	vector<T> toVector() {
		vector<T> v;
		for (auto it = mEnumerable->getEnumerator(); it->moveNext(); v.push_back(it->current()));
		return v;
	}

private:
	IEnumerablePtr<T> mEnumerable;
};

template<typename CT>
LinqObject<typename STLEnumerable<CT>::Type> from(CT const &c) {
	return LinqObject<typename STLEnumerable<CT>::Type>(make_shared<STLEnumerable<CT>>(c));
}

LinqObject<int> range(int end) {
	assert(end > 0);
	return LinqObject<int>(make_shared<RangeEnumerable<true>>(0, end, 1));
}

LinqObject<int> range(int begin, int end, int step = 1) {
	assert(step != 0);
	assert((step > 0 && end > begin) || (step < 0 && (end < begin)));
	return step > 0
		? LinqObject<int>(make_shared<RangeEnumerable<true>>(begin, end, step))
		: LinqObject<int>(make_shared<RangeEnumerable<false>>(begin, end, step));
}
//-------------------------------------------------------------------------
static void unitTest() {
	{
		int a[] = { 0, 1, 2, 3, 4 };
		assert(!from(a).empty());
		assert(from(string()).empty());
	}
	{
		assert(from("").size() == 1);
	}
	{
		assert(
			from(vector<int>{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
			.filter([](int i){ return i % 2 == 0; })
			.map([](int i){ return long(i) + i; })
			.reduce(0, [](long s, int i){ return s + i; }) == 40);
	}
	{
		assert(range(0, 10).reduce(0, [](int s, int i){ return s + i; }) == 45);
		assert(range(9, -1, -1).sum() == 45);
	}
	{
		assert(from(range(0, 10).toVector()).equals(range(9, -1, -1).reverse()));
	}
	{
		assert(from(vector<int>{3, 5, 7, 9}).find([](int i){return i % 5 == 2; }) == 7);
	}
	{
		assert(!from(vector<int>{3, 5, 7, 9, 4}).all([](int i){return i % 2 == 1; }));
		assert(from(vector<int>{3, 5, 7, 9}).all([](int i){return i % 2 == 1; }));
		assert(from(vector<int>{3, 5, 7, 9, 4}).any([](int i){return i % 2 == 0; }));
		assert(!from(vector<int>{3, 5, 7, 9}).all([](int i){return i % 2 == 0; }));
	}
	{
		assert(from(vector<int>{3, 1, 4, 0, 2}).sorted(less<int>()).equals(range(0, 5)));
	}
	{
		int a[] = { 3, 7, 5, 1, 2, 4, 6 };
		assert(from(a).take(3).equals(from(vector<int>{3, 7, 5})));
		assert(from(a).takeWhile([](int i){ return i > 1; }).sum() == 15);
		assert(from(a).dropWhile([](int i){ return i > 1; }).equals(from(a).drop(3)));
	}
}

int main() {
	unitTest();
}

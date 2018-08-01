#ifndef VCTRS_VCTRS_DETAIL_ATOMIC_VCTR_H
#define VCTRS_VCTRS_DETAIL_ATOMIC_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include <vctrs/typed_vctr.h>

namespace vctrs {
  namespace detail {

    using namespace Rcpp;

    template <class RCPP_TYPE>
    class AtomicVctr : public TypedVctr<AtomicVctr<RCPP_TYPE> > {
    public:
      AtomicVctr(SEXP x_) : x(x_) {}

    public:
      virtual size_t length() const {
        return Rf_length(x);
      }

      virtual Vctr* subset(const SlicingIndex& index) const {
        LogicalVector ret(index.size());

        for (size_t i = 0; i < index.size(); ++i) {
          ret[i] = x[index[i]];
        }

        return new AtomicVctr(ret);
      }

      virtual Vctr* combine(const Vctr& other) const {
        const AtomicVctr& my_other = static_cast<const AtomicVctr&>(other);

        RCPP_TYPE ret(x.length() + my_other.x.length());
        for (R_xlen_t i = 0; i < x.length(); ++i) {
          ret[i] = x[i];
        }
        for (R_xlen_t i = 0; i < my_other.x.length(); ++i) {
          ret[x.length() + i] = my_other.x[i];
        }
        return new AtomicVctr(ret);
      }

      virtual Vctr* clone() const {
        return new AtomicVctr(x);
      }

      virtual SEXP get_sexp() const {
        return x;
      }

    public:
      bool all_na() const {
        for (R_xlen_t i = 0; i < x.length(); ++i) {
          if (!x.is_na(x[i]))
            return false;
        }
        return true;
      }

    private:
      RCPP_TYPE x;
    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_ATOMIC_VCTR_H

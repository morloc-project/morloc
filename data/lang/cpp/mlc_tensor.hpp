#ifndef MLC_TENSOR_HPP
#define MLC_TENSOR_HPP

// mlc_tensor.hpp -- thin owning tensor built on std::vector + std::mdspan.
// Ownership: std::vector<S> (contiguous row-major storage).
// View: std::mdspan via .view() for multidimensional access.
//
// std::mdspan is a C++23 header. Until libstdc++ 15 is widely deployed we
// use the kokkos/mdspan reference implementation (vendored as mdspan.hpp),
// which configures itself to inject mdspan / dextents / extents / layout_*
// into namespace std. Once <mdspan> ships natively, drop the include and
// the user-facing names below remain unchanged.

#include <array>
#include <cstdint>
#include <numeric>
#include <stdexcept>
#include <vector>
#if __cpp_lib_mdspan >= 202207L
#  include <mdspan>
#else
#  include "mdspan.hpp"
#endif

namespace mlc {

// Storage type trait: maps bool to uint8_t so tensor memory layout
// matches the voidstar format (1 byte) regardless of sizeof(bool).
template<typename T> struct tensor_storage { using type = T; };
template<> struct tensor_storage<bool> { using type = uint8_t; };
template<typename T> using tensor_storage_t = typename tensor_storage<T>::type;

template<typename T, int NDim>
class Tensor {
    using S = tensor_storage_t<T>;
public:
    // Construct with given shape
    Tensor(const std::array<int64_t, NDim>& dims) : shape_(dims) {
        size_t n = 1;
        for (int i = 0; i < NDim; i++) n *= (size_t)shape_[i];
        data_.resize(n);
    }

    // Construct from flat data + shape
    Tensor(std::vector<S> data, const std::array<int64_t, NDim>& dims)
        : data_(std::move(data)), shape_(dims) {}

    // Construct from flat data + shape pointer (for deserialization)
    Tensor(const S* src, const int64_t* shape, size_t total)
        : data_(src, src + total) {
        for (int i = 0; i < NDim; i++) shape_[i] = shape[i];
    }

    Tensor() = default;
    Tensor(Tensor&&) = default;
    Tensor& operator=(Tensor&&) = default;
    Tensor(const Tensor&) = default;
    Tensor& operator=(const Tensor&) = default;

    // mdspan view (row-major / C order)
    auto view() {
        return std::mdspan<S, std::dextents<int64_t, NDim>>(data_.data(), shape_);
    }
    auto view() const {
        return std::mdspan<const S, std::dextents<int64_t, NDim>>(data_.data(), shape_);
    }

    // Raw access
    S* data() { return data_.data(); }
    const S* data() const { return data_.data(); }
    size_t size() const { return data_.size(); }
    const std::array<int64_t, NDim>& shape() const { return shape_; }
    int64_t shape(int d) const { return shape_[d]; }
    constexpr int ndim() const { return NDim; }

    // Linear access
    S& operator[](size_t i) { return data_[i]; }
    const S& operator[](size_t i) const { return data_[i]; }

    // Move out the backing vector (for unpack / serialization)
    std::vector<S>& storage() { return data_; }
    const std::vector<S>& storage() const { return data_; }

private:
    std::vector<S> data_;
    std::array<int64_t, NDim> shape_{};
};

// Convenience aliases
template<typename T> using Tensor1 = Tensor<T, 1>;
template<typename T> using Tensor2 = Tensor<T, 2>;
template<typename T> using Tensor3 = Tensor<T, 3>;
template<typename T> using Tensor4 = Tensor<T, 4>;
template<typename T> using Tensor5 = Tensor<T, 5>;

// Type traits
template<typename T> struct is_mlc_tensor : std::false_type {};
template<typename T, int N> struct is_mlc_tensor<Tensor<T, N>> : std::true_type {};
template<typename T> inline constexpr bool is_mlc_tensor_v = is_mlc_tensor<T>::value;

template<typename T> struct tensor_element;
template<typename T, int N> struct tensor_element<Tensor<T, N>> { using type = T; };
template<typename T> using tensor_element_t = typename tensor_element<T>::type;

template<typename T> struct tensor_ndim;
template<typename T, int N> struct tensor_ndim<Tensor<T, N>>
    { static constexpr int value = N; };
template<typename T> inline constexpr int tensor_ndim_v = tensor_ndim<T>::value;

} // namespace mlc

#endif // MLC_TENSOR_HPP

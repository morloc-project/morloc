#ifndef MLC_TENSOR_HPP
#define MLC_TENSOR_HPP

// mlc_tensor.hpp -- dense N-dimensional tensor for morloc C++ pools.
// Data is always contiguous row-major (C order). The Tensor struct in
// schema.h defines the voidstar layout; this header provides the C++
// user-facing type that maps to it.

#include "morloc.h"
#include <cstring>
#include <stdexcept>
#include <numeric>
#include <initializer_list>

namespace mlc {

// Storage type trait: maps bool to uint8_t so that tensor memory layout
// matches the voidstar format (MORLOC_BOOL = 1 byte) regardless of
// sizeof(bool) on the target platform.
template<typename T> struct tensor_storage { using type = T; };
template<> struct tensor_storage<bool> { using type = uint8_t; };
template<typename T> using tensor_storage_t = typename tensor_storage<T>::type;

template<typename T, int NDim>
class Tensor {
    using S = tensor_storage_t<T>;
public:
    // Construct with given shape, allocating data on the heap
    Tensor(const int64_t (&dims)[NDim]) : owns_data_(true) {
        for (int i = 0; i < NDim; i++) shape_[i] = dims[i];
        total_ = 1;
        for (int i = 0; i < NDim; i++) total_ *= (size_t)shape_[i];
        data_ = new S[total_]();
    }

    // Construct from initializer list of dims
    Tensor(std::initializer_list<int64_t> dims) : owns_data_(true) {
        if ((int)dims.size() != NDim) {
            throw std::runtime_error("Tensor dimension mismatch");
        }
        int i = 0;
        for (auto d : dims) shape_[i++] = d;
        total_ = 1;
        for (i = 0; i < NDim; i++) total_ *= (size_t)shape_[i];
        data_ = new S[total_]();
    }

    // Construct as a view over existing data (does not own)
    Tensor(S* data, const int64_t* shape, size_t total)
        : data_(data), total_(total), owns_data_(false) {
        for (int i = 0; i < NDim; i++) shape_[i] = shape[i];
    }

    ~Tensor() {
        if (owns_data_ && data_) delete[] data_;
    }

    // Move-only
    Tensor(Tensor&& other) noexcept
        : data_(other.data_), total_(other.total_), owns_data_(other.owns_data_) {
        for (int i = 0; i < NDim; i++) shape_[i] = other.shape_[i];
        other.data_ = nullptr;
        other.owns_data_ = false;
    }

    Tensor& operator=(Tensor&& other) noexcept {
        if (this != &other) {
            if (owns_data_ && data_) delete[] data_;
            data_ = other.data_;
            total_ = other.total_;
            owns_data_ = other.owns_data_;
            for (int i = 0; i < NDim; i++) shape_[i] = other.shape_[i];
            other.data_ = nullptr;
            other.owns_data_ = false;
        }
        return *this;
    }

    Tensor(const Tensor&) = delete;
    Tensor& operator=(const Tensor&) = delete;

    // Accessors (S* for raw access; S == T for all types except bool)
    const S* data() const { return data_; }
    S* data() { return data_; }
    constexpr int ndim() const { return NDim; }
    const int64_t* shape() const { return shape_; }
    int64_t shape(int d) const { return shape_[d]; }
    size_t size() const { return total_; }

    // Linear access (returns S& which is uint8_t& for bool tensors;
    // implicit conversion to/from bool handles the difference)
    const S& operator[](size_t i) const { return data_[i]; }
    S& operator[](size_t i) { return data_[i]; }

    // 1D access
    template<int N = NDim, typename = std::enable_if_t<N == 1>>
    const S& operator()(int64_t i) const { return data_[i]; }
    template<int N = NDim, typename = std::enable_if_t<N == 1>>
    S& operator()(int64_t i) { return data_[i]; }

    // 2D access (row-major)
    template<int N = NDim, typename = std::enable_if_t<N == 2>>
    const S& operator()(int64_t i, int64_t j) const {
        return data_[i * shape_[1] + j];
    }
    template<int N = NDim, typename = std::enable_if_t<N == 2>>
    S& operator()(int64_t i, int64_t j) {
        return data_[i * shape_[1] + j];
    }

    // 3D access (row-major)
    template<int N = NDim, typename = std::enable_if_t<N == 3>>
    const S& operator()(int64_t i, int64_t j, int64_t k) const {
        return data_[(i * shape_[1] + j) * shape_[2] + k];
    }
    template<int N = NDim, typename = std::enable_if_t<N == 3>>
    S& operator()(int64_t i, int64_t j, int64_t k) {
        return data_[(i * shape_[1] + j) * shape_[2] + k];
    }

private:
    S* data_ = nullptr;
    int64_t shape_[NDim] = {};
    size_t total_ = 0;
    bool owns_data_ = false;
};

// Convenience aliases
template<typename T> using Tensor1 = Tensor<T, 1>;
template<typename T> using Tensor2 = Tensor<T, 2>;
template<typename T> using Tensor3 = Tensor<T, 3>;
template<typename T> using Tensor4 = Tensor<T, 4>;
template<typename T> using Tensor5 = Tensor<T, 5>;

// Type trait for detecting mlc::Tensor
template<typename T> struct is_mlc_tensor : std::false_type {};
template<typename T, int N> struct is_mlc_tensor<Tensor<T, N>> : std::true_type {};
template<typename T>
inline constexpr bool is_mlc_tensor_v = is_mlc_tensor<T>::value;

// Extract element type from Tensor
template<typename T> struct tensor_element;
template<typename T, int N> struct tensor_element<Tensor<T, N>> { using type = T; };
template<typename T> using tensor_element_t = typename tensor_element<T>::type;

// Extract ndim from Tensor
template<typename T> struct tensor_ndim;
template<typename T, int N> struct tensor_ndim<Tensor<T, N>>
    { static constexpr int value = N; };
template<typename T>
inline constexpr int tensor_ndim_v = tensor_ndim<T>::value;

} // namespace mlc

#endif // MLC_TENSOR_HPP

using Lazy

# Pair structure
pair(A, B) = (sel -> sel(A, B))
fst(A, B) = A
snd(A, B) = B

## Usage
pair_1_2 = pair(1, 2)
@assert pair_1_2(fst) == 1
@assert pair_1_2(snd) == 2

# Conditionals
cnd(C, T, E) = C(T, E)
truth(A, B) = A()
falsity(A, B) = B()

## Usage
cnd(truth,  () -> 1,  () -> error("BAD"))
cnd(falsity,  () -> 1,  () -> 2)

# Factorial -> How to express it only with function calls
function fact(n)
    if iszero(n)
        1
    else
        n * fact(sub1(n))
    end
end

add1(n) = n + one(n)
sub1(n) = n - one(n)

# Represent numbers
z = f -> (x -> x)
succ(N) = f -> (x -> N(f)(x) |> f)

n_to_i(N) = N(add1)(0) 
i_to_n(i) = iszero(i) ? z : (sub1(i) |> i_to_n |> succ)

## Define few numbers
Num = i_to_n.(1:10)
@assert n_to_i(Num[3]) == 3

# M + N = 1 + ... + 1 + N
add(M) = N -> M(succ)(N)

@assert add(Num[2])(Num[3]) |> n_to_i == 5
@assert add(Num[2])(z) |> n_to_i == 2

# M * N = M + ... + M
mult(M) = N -> N(add(M))(z)

@assert mult(Num[2])(Num[6]) |> n_to_i == 12
@assert mult(z)(Num[6]) |> n_to_i == 0

# express iszero with lambda's 
is_zero(N) = N((_) -> falsity)(truth)

@assert is_zero(z) == truth
@assert is_zero(Num[1]) == falsity
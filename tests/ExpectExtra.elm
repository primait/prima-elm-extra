module ExpectExtra exposing
    ( shouldBehaveLike
    , shouldBehaveLike2
    , shouldBehaveLike3
    )

import Expect


shouldBehaveLike : (a -> b) -> (a -> b) -> a -> Expect.Expectation
shouldBehaveLike f g x =
    Expect.equal (f x) (g x)


shouldBehaveLike2 : (a -> b -> c) -> (a -> b -> c) -> a -> b -> Expect.Expectation
shouldBehaveLike2 f g x y =
    Expect.equal (f x y) (g x y)


shouldBehaveLike3 : (a -> b -> c -> d) -> (a -> b -> c -> d) -> a -> b -> c -> Expect.Expectation
shouldBehaveLike3 f g x y z =
    Expect.equal (f x y z) (g x y z)

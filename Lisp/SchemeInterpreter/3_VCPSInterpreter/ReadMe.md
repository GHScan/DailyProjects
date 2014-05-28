#### Introduction

The most important difference between VCPS and CPS is the first one do not require the interpreter support tail recursion optmization.
Instead of invoke continuation directly like `(k value)`, VCPS defered the invocation with `(cons k value)`, eval procedure should repeatly consume the pair until got an empty continuation, then the cdr of pair is result.

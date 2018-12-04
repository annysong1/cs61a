""" Lab 3: Recursion and Python Lists """

# Q1
def skip_add(n):
    """ Takes a number x and returns x + x-2 + x-4 + x-6 + ... + 0.

    >>> skip_add(5)  # 5 + 3 + 1 + 0
    9
    >>> skip_add(10) # 10 + 8 + 6 + 4 + 2 + 0
    30
    >>> # Do not use while/for loops!
    >>> from construct_check import check
    >>> check('lab03.py', 'skip_add',
    ...       ['While', 'For'])
    True
    """
    if n <= 0:
        return 0
    else:
        return n + skip_add(n-2)

# Q2
def hailstone(n):
    """Print out the hailstone sequence starting at n, and return the
    number of elements in the sequence.

    >>> a = hailstone(10)
    10
    5
    16
    8
    4
    2
    1
    >>> a
    7
    >>> # Do not use while/for loops!
    >>> from construct_check import check
    >>> check('lab03.py', 'hailstone',
    ...       ['While', 'For'])
    True
    """
    print(n)
    if n == 1:
        return 1
    elif n % 2 == 0:
        n //= 2
    else:
        n = (n * 3) + 1
    return hailstone(n) + 1


# Q3
def if_this_not_that(i_list, this):
    """Define a function which takes a list of integers `i_list` and an integer
    `this`. For each element in `i_list`, print the element if it is larger
    than `this`; otherwise, print the word "that".

    >>> original_list = [1, 2, 3, 4, 5]
    >>> if_this_not_that(original_list, 3)
    that
    that
    that
    4
    5
    """
    i = 0

    while i < len(i_list):
        if i_list[i] > this:
            print(i_list[i])
        else:
            print("that")
        i += 1 










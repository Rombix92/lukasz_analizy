#Create an decorator which takes an argument of number, if number 56 is apply to this decorater than decorator allows for printing function below decorator
#with extra string before and after result of given function.

import functools



def decorator_with_number(number):
    def decorator(func):
        functools.wraps(func)
        def function_that_wraps_func():
            print("Begining of decorator")
            if number == 56:
                func()
            else:
                print("I don't print function")
            print("End of decorator")
        return function_that_wraps_func
    return decorator



@decorator_with_number(57)
def my_function():
    print("Hello world")

my_function()

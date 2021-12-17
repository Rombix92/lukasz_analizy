# This code present two functions:
#1) asking about set of known people
#2) again asking for one name and checking whether this name consist in list of known names

def who_do_you_know():
    names=input("Write the names of people you know (separate names by commas):")

""" below list comprehension allow for very useful list creation. It should be read from the right, it takes inputed values first and"""
""" separate it by comma, then takes each element of this list and remove space, tabs etc (strip) and transform each string to lower cases"""

    normalized_list_of_people=[person.strip().lower() for person in names.split(',')]
    return normalized_list_of_people

def ask_user():
    name=input("Give me some name:")
    if name in who_do_you_know():
        print("You know {}!".format(name))

ask_user()

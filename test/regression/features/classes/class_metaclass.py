class Meta(type):
    def __init__(self, class_name, bases, namespace):
        print(class_name, bases, namespace) 

class C(metaclass=Meta):
    pass

C()

/* Test struct pointers and arrow operator */
#include <stdio.h>

struct Person {
    char name[20];
    int age;
};

void print_person(struct Person *p) {
    printf("Name: %s, Age: %d\n", p->name, p->age);
}

int main() {
    struct Person person1 = {"Alice", 25};
    struct Person person2 = {"Bob", 30};
    struct Person *ptr = &person1;
    
    printf("Direct access: ");
    printf("Name: %s, Age: %d\n", person1.name, person1.age);
    
    printf("Pointer access: ");
    printf("Name: %s, Age: %d\n", ptr->name, ptr->age);
    
    printf("Via function: ");
    print_person(&person2);
    
    ptr = &person2;
    ptr->age = 31;
    printf("After ptr->age = 31: ");
    print_person(ptr);
    
    return 0;
}
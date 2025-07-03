/* Test nested structures */
#include <stdio.h>

struct Address {
    char street[30];
    int number;
};

struct Person {
    char name[20];
    int age;
    struct Address address;
};

int main() {
    struct Person person = {
        "John Doe",
        35,
        {"Main Street", 123}
    };
    
    printf("Person: %s, Age: %d\n", person.name, person.age);
    printf("Address: %d %s\n", person.address.number, person.address.street);
    
    person.address.number = 456;
    printf("New address: %d %s\n", person.address.number, person.address.street);
    
    return 0;
}
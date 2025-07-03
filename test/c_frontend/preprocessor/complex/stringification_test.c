/* Test stringification operator */
#define STRINGIFY(x) #x
#define MAKE_STRING(name) char* str_##name = #name;

int main() {
    MAKE_STRING(hello)
    MAKE_STRING(world)
    
    char* test1 = STRINGIFY(foo);
    char* test2 = STRINGIFY(123);
    char* test3 = STRINGIFY(a + b);
    
    return 0;
}
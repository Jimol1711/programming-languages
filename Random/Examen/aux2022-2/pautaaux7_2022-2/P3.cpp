#include <iostream>
using namespace std;

long double_rec (long value, long accum){
    if (value == 0)
        return accum;
    else
        return double_rec(value-1, accum+2);
}

int main() {
	long number, res;
	cout << "Enter a natural number: ";
	cin >> number;
	res = double_rec(number, 0);
	cout << "Its double is: " << res << "\n";
	return 0;
}
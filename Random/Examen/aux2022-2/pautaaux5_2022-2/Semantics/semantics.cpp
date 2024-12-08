#include <iostream>
using namespace std;

int getNum5(int x) {
    return 5;
}

int getHii7() {
    cout<<"Hii\n";
    return 7;
}

int main() {
	cout<<getNum5(getHii7())<<"\n";
	return 0;
}
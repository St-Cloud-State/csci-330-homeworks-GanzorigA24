#include <iostream>
#include <stack>
#include <vector>
using namespace std;

// Partition function
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Choose the last element as the pivot
    int i = low - 1;       // Index of the smaller element

    // Traverse the array and rearrange elements
    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++; // Increment the index of the smaller element
            swap(arr[i], arr[j]); // Swap arr[i] and arr[j]
        }
    }
    // Place the pivot in the correct position
    swap(arr[i + 1], arr[high]);
    return i + 1; // Return the partition index
}

// Iterative Quicksort
void quicksort(vector<int>& arr, int low, int high) {
    stack<int> stk; // Use a stack to simulate recursion
    stk.push(low);  // Push the initial low index
    stk.push(high); // Push the initial high index

    // Continue until the stack is empty
    while (!stk.empty()) {
        high = stk.top(); stk.pop(); // Pop the high index
        low = stk.top(); stk.pop();  // Pop the low index

        int p = partition(arr, low, high); // Partition the array

        // If there are elements on the left side of the pivot, push them to the stack
        if (p - 1 > low) {
            stk.push(low);
            stk.push(p - 1);
        }

        // If there are elements on the right side of the pivot, push them to the stack
        if (p + 1 < high) {
            stk.push(p + 1);
            stk.push(high);
        }
    }
}

int main() {
    vector<int> arr = {9, 10, 8, 2, 1, 6}; // Input array
    quicksort(arr, 0, arr.size() - 1);     // Sort the array

    cout << "Sorted array: ";
    for (int i : arr) cout << i << " "; // Print the sorted array
    return 0;
}
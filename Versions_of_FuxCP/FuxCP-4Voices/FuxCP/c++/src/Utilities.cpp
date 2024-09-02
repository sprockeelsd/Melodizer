// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the implementations of general utility functions. 
//

#include "../headers/Utilities.hpp"

/**
 * For a given set of intervals between notes that loops and a starting note, returns all the possible notes
 * @param note the starting note
 * @param intervals the set of intervals between notes. It must make a loop. For example, to get all notes from a major
 * scale from note, use {2, 2, 1, 2, 2, 2, 1}. To get all notes from a minor chord, use {3, 4, 5}.
 * @return vector<int> all the notes
 */
vector<int> get_all_notes_from_interval_loop(int n, vector<int> intervals)
{
    int note = n % PERFECT_OCTAVE; // bring the root back to [12,23] in case the argument is wrong
    vector<int> notes;

    int i = 0;
    while (note <= 127)
    {
        notes.push_back(note);
        note += intervals[i % intervals.size()];
        ++i;
    }
    return notes;
}

/**
 * For a given tonality (root + mode), returns all the possible notes
 * @param root the root of the tonality (in [0,11])
 * @param scale the set of tones and semitones that define the scale
 * @return vector<int> all the possible notes from that tonality
 */
vector<int> get_all_notes_from_scale(int root, vector<int> scale)
{
    return get_all_notes_from_interval_loop(root, scale);
}

/**
 * For a given chord (root + mode), returns all the possible notes
 * @param root the root of the chord
 * @param quality the set of tones and semitones that define the chord
 * @return vector<int> all the possible notes from that chord
 */
vector<int> get_all_notes_from_chord(int root, vector<int> quality)
{
    return get_all_notes_from_interval_loop(root, quality);
}

/**
 * Get all values for a given note
 * @param note a note
 * @return vector<int> a vector containing all the given notes
 */
vector<int> get_all_given_note(int note)
{
    int current = note % PERFECT_OCTAVE;
    vector<int> notes;
    while (current < 127)
    {
        notes.push_back(current);
        current += 12;
    }
    return notes;
}

/**
 * Transforms a vector of integers into a string
 * @param vector a vector of integers
 * @return string the string representation of the vector
 */
string int_vector_to_string(vector<int> vector){
    string s;
    for (int i = 0; i < vector.size(); ++i) {
        s += to_string(vector[i]);
        if(i != vector.size() - 1)
            s += " , ";
    }
    return s;
}

/**
 * Transforms an int* into a vector<int>
 * @param ptr an int* pointer
 * @param size the size of the array
 * @return a vector<int> containing the same values as the array
 */
vector<int> int_pointer_to_vector(int* ptr, int size){
    vector<int> v;
    for(int i = 0; i < size; i++){
        v.push_back(ptr[i]);
    }
    return v;
}

/**
 * Union algorithm found and adapted from GeeksForGeeks.com
 */
vector<int> vector_union(vector<int> v1, vector<int> v2){
    vector<int> v(v1.size() 
                  + v2.size()); 
    vector<int>::iterator it, st; 

    it = set_union(v1.begin(), v1.end(), 
                   v2.begin(), 
                   v2.end(), v.begin()); 

    vector<int> res = {};
    for(st = v.begin(); st != it; ++st){
        res.push_back(*st);
    }
    return res;
}

/**
 * Intersection algorithm found and adapted from GeeksForGeeks.com
 */
vector<int> vector_intersection(vector<int> v1, vector<int> v2){
    vector<int> v(v1.size() 
                  + v2.size()); 
    vector<int>::iterator it, st; 

    it = set_intersection(v1.begin(), v1.end(), 
                   v2.begin(), 
                   v2.end(), v.begin()); 

    vector<int> res = {};
    for(st = v.begin(); st != it; ++st){
        res.push_back(*st);
    }
    return res;
}

/**
 * Difference algorithm found and adapted from GeeksForGeeks.com
 */
vector<int> vector_difference(vector<int> v1, int lb, int ub){
    vector<int> v = {};
    for(int i = lb; i <= ub; i++){
        int t = 0;
        for(int j = 0; j < v1.size(); j++){
            if(i==v1[j]){
                t=1;
                break;
            }
        }
        if(t==0){
            v.push_back(i);
        }
    }
    return v;
}

/**
 * Prints the Search::Statistics object into a readable format
 * @param stats a Search::Statistics object representing the statistics of a search
 * @return The string representation of the statistics object
 */
string statistics_to_string(Search::Statistics stats){
    string s = "Nodes traversed: " + to_string(stats.node) + "\n";
    s += "Failed nodes explored: " + to_string(stats.fail) + "\n";
    s += "Restarts performed: " + to_string(stats.restart) + "\n";
    s += "Propagators executed: " + to_string(stats.propagate) + "\n";
    s += "No goods generated: " + to_string(stats.nogood) + "\n";
    s += "Maximal depth of explored tree: " + to_string(stats.depth) + "\n";
    return s;
}

/**
 * Prints the Search::Statistics object into a csv format (coma separated)
 * @param stats a Search::Statistics object representing the statistics of a search
 * @return The string representation of the statistics object
 */
string statistics_to_csv_string(Search::Statistics stats){
    string s = to_string(stats.node) + ",";
    s += to_string(stats.fail) + ",";
    s += to_string(stats.restart) + ",";
    s += to_string(stats.propagate) + ",";
    s += to_string(stats.nogood) + ",";
    s += to_string(stats.depth) + ",";
    return s;
}

/**
 * Returns the value of a variable as a string
 * @param var an integer variable
 * @param absolute a boolean indicating if the value should be returned as an absolute value (default is false)
 * @return a string representing the value of the variable
 */
string intVar_to_string(const IntVar &var, bool absolute) {
    if (var.assigned()){
        if(absolute)
            return to_string(abs(var.val()));
        return to_string(var.val());
    }
    return "<not assigned>";
}

/**
 * Returns the values of an array of variables as a string
 * @param vars an array of integer variables
 * @return a string representing the values of the variables
 */
string intVarArray_to_string(IntVarArray vars){
    int s = vars.size();
    string res = "{";
    for(int i = 0; i < s; i++){
        res += intVar_to_string(vars[i]);
        if(i != s - 1)
            res += ", ";
    }
    res += "}";
    return res;
}

/**
 * Returns the value of a variable as a string
 * @param var an integer variable
 * @param absolute a boolean indicating if the value should be returned as an absolute value (default is false)
 * @return a string representing the value of the variable
 */
string boolVar_to_string(const BoolVar &var, bool absolute) {
    if (var.assigned()){
        if(absolute)
            return to_string(abs(var.val()));
        return to_string(var.val());
    }
    return "<not assigned>";
}

/**
 * Returns the values of an array of variables as a string
 * @param vars an array of integer variables
 * @return a string representing the values of the variables
 */
string boolVarArray_to_string(BoolVarArray vars){
    int s = vars.size();
    string res = "{";
    for(int i = 0; i < s; i++){
        res += boolVar_to_string(vars[i]);
        if(i != s - 1)
            res += ", ";
    }
    res += "}";
    return res;
}

/**
 * Returns the values of an array of variables as a string
 * @param vars an array of integer variables
 * @return a string representing the values of the variables, ignoring unassigned variables
 */
string cleanIntVarArray_to_string(IntVarArray vars){
    int s = vars.size();
    string res = "{";
    for(int i = 0; i < s; i++){
        if (vars[i].assigned()){
            res += intVar_to_string(vars[i]);
            if(i != s - 1)
                res += ", ";
        }
    }
    res += "}";
    return res;
}

/**
 * Returns the values of an IntVarArgs as a string
 * @param args an IntVarArgs
 * @return a string representing the values
 */
string intVarArgs_to_string(IntVarArgs args){
    int s = args.size();
    string res;
    for(int i = 0; i < s; i++){
        res += intVar_to_string(args[i], 0);
        if(i != s - 1)
            res += ", ";
    }
    return res;
}



/**
 * Returns the name of a note based on its MIDI value
 * @param note an integer
 */
string midi_to_letter(int note){
    return noteNames[note % PERFECT_OCTAVE];
}

/**
 * Returns the name of a mode based on its integer value
 * @param mode an integer
 * @return a string representing the name of the mode
 */
string mode_int_to_name(int mode){
    return modeNames[mode];
}

/**
 * returns a string with the time
 * @return a string with the time
 */
string time(){
    /// date and time for logs
    std::time_t currentTime = std::time(nullptr); // Get the current time
    std::string timeString = std::asctime(std::localtime(&currentTime)); // Convert to string
    return timeString;
}

/**
 * Write a text into a log file
 * Useful for debugging in the OM environment
 * @param message the text to write
 */
void write_to_log_file(const char *message, const string& filename) { // TODO : TO ACTIVATE LOGGING, UNCOOMMENT THE CONTENTS OF THIS FUNCTION AND THE NEXT FUNCTION.
    // const char* homeDir = std::getenv("HOME"); // Get the user's home directory
    // if (homeDir) {
    //     std::string filePath(homeDir);
    //     filePath += "/Documents/Libraries/MusicConstraints/out/" + filename; // Specify the desired file path, such as $HOME/log.txt

    //     std::ofstream myfile(filePath, std::ios::app); // append mode
    //     if (myfile.is_open()) {
    //         myfile << message << endl;
    //         myfile.close();
    //     }
    // }
}

/**
 * Write a text into a log file
 * @param message the text to write
 */
void writeToLogFile(const char* message){ // TODO : TO ACTIVATE LOGGING, UNCOOMMENT THE CONTENTS OF THIS FUNCTION AND THE PREVIOUS FUNCTION.
    // std::time_t currentTime = std::time(nullptr); // Get the current time
    // std::string timeString = std::asctime(std::localtime(&currentTime)); // Convert to string

    // const char* homeDir = std::getenv("HOME"); // Get the user's home directory
    // if (homeDir) {
    //     std::string filePath(homeDir);
    //     filePath += "/log.txt"; // Specify the desired file path, such as $HOME/log.txt

    //     std::ofstream myfile(filePath, std::ios::app); // append mode
    //     if (myfile.is_open()) {
    //         myfile <<timeString<< endl << message << endl;
    //         myfile.close();
    //     }
    // }
}
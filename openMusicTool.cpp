#include "OpenMusicTool.h"

#include "Event.cpp"

using namespace Gecode;
using namespace std;

/*representation used : 
    - MIDI notes range from 21 (A0) to 108 (C8) with 20 a rest

    - Rythm : duration expressed in sixteenth notes : 1 = sixteenth note, 2 = eigth note, 3 = dotted eigth note, 4 = quarter note, 
                                                      6 = dotted quarter note, 8 = half note, 12 = dotted half note, 16 = whole note.

    - Position : expressed in sixteenth notes from the beginning of the piece(example : first beat of second measure is 17)

    - Rythm tree structure in OM = (Nb of mesures ((time signature) (rythmic proportions)))
    example = (2 (
        (
            (4 4) (1 1 1 1)
        ) (
            (4 4) (1 1 1 1)
          )
        )
    )
*/

//constraint for rythm : no overlap with notes as rectangles, unary constraint

// currently we only take care of major/minor chords, not augmented nor diminished chords
 
//constraints for notes : precedence (one before the other), 
  //intervalle entre note haute de l'accord et mélodie < une octave (12), pas septième(10-11) ni quarte augmentée(6)
  //sur les accords majeurs/mineurs : doubler la fondamentale en priorité, puis la quinte, puis la tierce
  //sur les accords diminués, doubler la tierce
  //sur les accords augmentés, doubler la fondamentale ou la tierce
  //quintes et octaves parralèles interdites
  //intervalles de seconde à éviter
  //mouvements contraires et obliques à privilégier
  //saut à l'octave précédé et suivi d'un mouvement contraire

class OpenMusicTool : public Space {
  protected:
    //Melody
    IntVarArray pitch;  //the pitch 
    vector<int> duration;    //the duration of a note expressed in sixteenth notes
    vector<int> start;  //the start of the note with respect to the start of the piece expressed in sixteenth notes
    
    //Chords
    const vector<vector<int>> chords;   //the list of input chords
    const vector<int> chordDuration;    //the duration of the chords
    const vector<int> chordStart;   //the starting point of the chords

    int n;  //the number of notes
    int scale[7]; //The scale corresponding to the key and the mode

    //intervals depending on the mode
    const int majorScale[7] = {2, 2, 1, 2, 2, 2, 1};
    const int minorScale[7] = {2, 1, 2, 2, 1, 2, 2};

    //sensitive notes
    vector<int> firsts;
    vector<int> seconds;
    vector<int> thirds;
    vector<int> fourths;
    vector<int> fifths;
    vector<int> sixths;
    vector<int> sevenths;

    //admissible intervals with the highest note of the chord
    const vector<int> admissibleIntervalsWithHighNoteFromChord{3, 4, 5, 7, 8, 9, 12,
                                                              -3, -4, -5, -8, -9, -12}; //only positive because the note can't be below the highest note in the chord

    int minimum;

  public:
    /*
      dissonnance resolution : les sensibles (4ème ou 7ème) vont vers la tonique (uniquement en haut pour les 7èmes)
      - if a note is a seventh or a fourth for the scale, the next note has to be the key
      - if it is a seventh, the following note is the key above (+1 for major or +2 for minor)
      - if it is a fourth, then it can be above (+7) or below (-5)
    */
    void dissonnanceResolution(int key, bool major){//works but only for sevenths note, note for fourths
      IntVarArgs temp(pitch.slice(pitch.size() -1, -1)); //reverse the array to enforce subsedience constraint with precedence constraint
      for(int i = 0; i < sevenths.size(); i++){
        precede(*this, temp, sevenths[i] + 1, sevenths[i]); //if there is a seventh, there will eventually be a tonic after it
      }    
    }
    /*
      the maximum interval between two adjacent notes is an octave and is one of the possible values
    */
    void interval(){//doesn't work rn
      
    }
    /*
      sur les accords majeurs/mineurs : doubler la fondamentale en priorité, puis la quinte, puis la tierce
    */
    void noteOnChord(){
      for(int i = 0; i < chords.size(); i++){//for each chord 
        for(int j = 0; j < start.size(); j++){//for each note
          if(chordStart[i] == start[j]){//the note is played at the same time as the chord
            const vector<int> vect({chords[i][0]+12, chords[i][1] + 12, chords[i][2] + 12}); //that note is either the tonic, third or fifth of the chord
            IntSet set(vect);
            dom(*this, pitch[j], set);
          }
        }
      }
    }
    /*
      get the seventh notes
    */
    void getSevenths(int key, bool major){//tested
      vector<int> seven(0);
      int note = key;
      
      int interval;
      if(major){
        interval = 11;
      }
      else{
        interval = 10;
      }
      note = note + interval;
      while(note < 108){
        seven.push_back(note);
        //std::cout << note << "\n";
        note = note + 12;
      }
      note = key - (12 - interval);
      while(note > 21){
        seven.push_back(note);
        //std::cout << note << "\n";
        note = note - 12;
      }
      sevenths = seven;
    }   
    /*
      tells if a value is a seventh
    */
    bool inSevenths(int value){//tested
      for(int i = 0; i < sevenths.size(); i++){
        if(sevenths[i] == value){
          return true;
        }
      }
      return false;
    }
    /*
      get the fourth notes
    */
    void getFourths(int key){//tested
      vector<int> four(0);
      int note = key+5;
      while(note < 108){
        four.push_back(note);
        //std::cout << note << "\n";
        note = note + 12;
      }
      note = key -7;
      while(note > 21){
        four.push_back(note);
        //std::cout << note << "\n";
        note = note - 12;
      }

   }
    /*
      tells if a value is a fourth
    */
    bool inFourths(int value){//tested
      for(int i = 0; i < fourths.size(); i++){
        if(fourths[i] == value){
          return true;
        }
      }
      return false;
    }    
    /*
      the notes are in the tonality specified
    */
    void inTonality(int key, bool major){  //tested
      vector<int> notes(0);
      int note = key;
      notes.push_back(note);

      if(major){
        for(int i = 0; i < 7; i++){
          scale[i] = majorScale[i];
        }
      }
      else{  //minor
        for(int i = 0; i < 7; i++){
          scale[i] = minorScale[i];
        }
      }
      //only keep the notes above the key in the tonality
      int pos = 0;
      while(note < 108){   //jusque la note la plus haute
        if(pos >= 7){       //quand on arrive à l'octave suivante on remet major/minor à 0
          pos = 0;
        }
        note = note + scale[pos];
        //std::cout << note << "\n";
        notes.push_back(note); //ajouter au domaine possible la valeur suivante
        ++pos;
      }
      //only keep the notes below the key in the tonality
      note = key;
      pos = 0;
      while(note > minimum){
        if (pos >= 7){
          pos = 0;
        }
        note = note - scale[(7 - pos - 1)];
        //std::cout << note << "\n";
        notes.push_back(note);
        ++pos;
      }
      //update le domaine
      const vector<int> notess = notes;
      IntSet set(notess);
      dom(*this, pitch, set);
    }
    /*
      the notes in the melody are not notes in the chord that is played at the same time and are above notes in the chord
    */
    void restrainDomain(){//tested
      //std::cout << "aboveChord, chords.size() = " << chords.size() << "\n";
      int j = 0;
      for(int i = 0; i < chords.size(); i++){//parcourir tous les accords
        //std::cout << "chord no : " << i << "\n";
        while(start[j] < chordStart[i] + chordDuration[i] - 1 && j < n){//pour chaque note jouée en même temps que l'accord
          //std::cout << "note n : " << j << "\n";
          dom(*this, pitch[j], chords[i][2] + 1, chords[i][2] + 12);  //la note jouée en même temps doit être au dessus de l'accord à maximum une octave
          ++j;
        }
      }
    }
    
    /*
      Constructor : 
      chords are given with the notes in ascending order
      the key is given in MIDI notation from C5 to B5(example : C = 60)
      if the tonality is major then major is true, else false
    */
    OpenMusicTool(vector<vector<int>> chordSequence, vector<int> chordDurationSequence, vector<int> chordStartSequence, 
                  vector<int> notesDurations, vector<int> notesStarts, int key, bool major) : pitch(*this,6, 21, 108), 
                  start(notesStarts), chords(chordSequence), chordStart(chordStartSequence), chordDuration(chordDurationSequence), 
                  n(notesStarts.size()) {  //pour l'instant, 2 mesures, 4 accords, 8 noires
      
      //get sevenths and fourths
      getSevenths(key, major);
      getFourths(key);

      //notes played with a chord are not notes in that chord and are above it
      restrainDomain();

      //all notes in the scale
      inTonality(key, major);

      //the note played at the same time as the chord is the tonic, third or fifth of the chord
      noteOnChord();

      //the interval between two adjacent note is valid
      //interval();

      //notes played during a certain chord is played have to respect the admissible intervals with the highest note of the chord

      // all notes distinct
      //distinct(*this, pitch);
      
      //every sensitive note is followed by the key
      //dom(*this, pitch[2], 71);
      dissonnanceResolution(key, major);


      // post branching
      branch(*this, pitch, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }
    // search support
    OpenMusicTool(OpenMusicTool& s) : Space(s) {
      pitch.update(*this, s.pitch);
    }
  
    virtual Space* copy(void) {
      return new OpenMusicTool(*this);
    }
  
    // print solution
    void print(void) const {
      std::cout << pitch << std::endl;
    }
};

// main function
int main(int argc, char* argv[]) {
  //chords passed as an argument, for now 
  vector<int> chord1{55,60,64}; //CM
  vector<int> chord2{57,60,65}; //FM
  vector<int> chord3{59,62,67}; //GM
  vector<int> chord4{55,60,64}; //CM

  vector<vector<int>> chordSeq{chord1, chord2, chord3, chord4};
  vector<int> chordDur{8, 8, 8, 8};
  vector<int> chordStar{0,8,16,24};

  vector<int> inputDurations{8, 6, 2, //mesure 1
                          6, 2, 8};   //mesure 2
  vector<int> inputStarts{0, 8, 14, 16, 22, 24};



  // create model and search engine
  OpenMusicTool* m = new OpenMusicTool(chordSeq, chordDur, chordStar, inputDurations, inputStarts, 60, true);
  DFS<OpenMusicTool> e(m);
  delete m;
  int nbSol = 0;
  // search and print all solutions
  while (OpenMusicTool* s = e.next()) {
    s->print(); delete s;
    ++nbSol;
  }
  std::cout << "number of solutions : " << nbSol << "\n";
  return 0;
}
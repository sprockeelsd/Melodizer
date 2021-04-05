#include "OpenMusicTool.h"

#include "Event.cpp"

using namespace Gecode;
using namespace std;

/*
    representation used : 
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
    IntVarArray pitch;  //the pitch 
    vector<int> duration;    //the duration of a note expressed in sixteenth notes
    vector<int> start;  //the start of the note with respect to the start of the piece expressed in sixteenth notes
    int n;  //the number of notes
    int scale[7];

    //scales
    const int majorScale[7] = {2, 2, 1, 2, 2, 2, 1};
    const int minorScale[7] = {2, 1, 2, 2, 1, 2, 2};

    //sensitive notes
    vector<int> sevenths;
    vector<int> fourths;

    //admissible intervals with the highest note of the chord
    const vector<int> admissibleIntervalsWithHighNoteFromChord{3, 4, 5, 7, 8, 9, 12}; //only positive because the note can't be below the highest note in the chord

    int minimum;

  public:

    /*
      dissonnance resolution : les sensibles (4ème ou 7ème) vont vers la tonique (uniquement en haut pour les 7èmes)
      - if a note is a seventh or a fourth for the scale, the next note has to be the key
      - if it is a seventh, the following note is the key above (+1 for major or +2 for minor)
      - if it is a fourth, then it can be above (+7) or below (-5)
    */
    void dissonnanceResolution(int key, bool major){
      for(int i = 0; i < 8-1; i = i + 1){     //n-1 because if the last note is a seventh or a fourth then it's ok
      std::cout << "dissonnance resolution\n";
        if(inSevenths(pitch[i].val())){
          std::cout << "dissonnance resolution : seventh\n";
          //assigner la valeur suivante à pitch[i].max() + scale[6]
          dom(*this, pitch[i+1], pitch[i].max()+scale[6]);
        }
        if(inFourths(pitch[i].val())){
          std::cout << "dissonnance resolution : seventh\n";
          //assigner la valeur suivante à pitch[i].max() + 7 ou -5
          const vector<int> t{(pitch[i].max()-5),(pitch[i].max()+7)};
          IntSet set(t);
          dom(*this, pitch[i+1], set);
        }
      }      
    }
    /*the maximum interval between two adjacent notes is an octave*/
    void maxInterval(){

    }  
    /*
      the notes in the melody are not below the lowest high note from chord
    */
    void aboveMin(IntVarArray l, int chord1[], int chord2[], int chord3[], int chord4[]){ //tested
      minimum = min(chord1[0], min(chord2[0],min(chord3[0], chord4[0])));
      //all notes above the lowest note in the chord
      dom(*this,l,minimum,108);
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
    bool inSevenths(int value){
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
      tells if a value is a seventh
    */
    bool inFourths(int value){
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
      the notes in the melody are not notes in the chord that is played at the same time
    */
    void notInChord(IntVar note1, IntVar note2, int chord[]){   //tested
      int chordSize = sizeof(chord)/sizeof(chord[0]);
      for(int i = 0; i < chordSize; i++){
        rel(*this,note1,IRT_NQ,chord[i]);
        rel(*this,note2,IRT_NQ,chord[i]);
      }
    }
    
    /*
      Main function : 
      chords are given with the notes in ascending order
      the key is given in MIDI notation from C5 to B5(example : C = 60)
      if the tonality is major then major is true, else false
    */
    OpenMusicTool(int chord1[], int chord2[], int chord3[], int chord4[], int key, bool major) : pitch(*this,8,21,108) {  //pour l'instant, 2 mesures, 4 accords, 8 noires
      
      n = 8;
      //size of a chord
      int chord1Size = sizeof(chord1)/sizeof(chord1[0]);

      //get sevenths and fourths
      getSevenths(key, major);
      getFourths(key);

      dom(*this, pitch[0], 59);
      //all notes above the lowest note in the chords
      aboveMin(pitch, chord1, chord2, chord3, chord4);
      //all notes in the scale
      inTonality(key, major);

      //first 2 notes are not in chord1, 3rd and 4th node note in chord2,...
      notInChord(pitch[0], pitch[1], chord1);
      notInChord(pitch[2], pitch[3], chord2);
      notInChord(pitch[4], pitch[5], chord3);
      notInChord(pitch[6], pitch[7], chord4);

      // all notes distinct
      //distinct(*this, pitch);
      
      //every sensitive note is followed by the key
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
  int chord1[3] = {55,60,64}; //CM
  int chord2[3] = {57,60,65}; //FM
  int chord3[3] = {59,62,67}; //GM
  int chord4[3] = {55,60,64}; //CM

  // create model and search engine
  OpenMusicTool* m = new OpenMusicTool(chord1, chord2, chord3, chord4, 60, true);
  DFS<OpenMusicTool> e(m);
  delete m;
  // search and print all solutions
  while (OpenMusicTool* s = e.next()) {
    s->print(); delete s;
  }
  return 0;
}
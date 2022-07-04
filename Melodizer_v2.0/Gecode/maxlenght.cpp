#include <gecode/driver.hh>//mel2
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/int/arithmetic.hh>
#include <gecode/set.hh>
#include <vector>

using namespace Gecode;

namespace {
  int majorNatural[7] = {2, 2, 1, 2, 2, 2, 1};
  int minorNatural[7] = {2, 1, 2, 2, 2, 1, 2};

  int progression[4] = {1, 5, 6, 4};
};

class Melody : public Script {
private:

  const int bars = 4;
  const int quantification = 48;

  const int chord_notes = 3;// to begin with we will concentrate in chords of three notes, seventh chords will come later
  //const int key = 21;// from 21(A0) to 108(C8)

  const int chords_per_bar = 2;

  //const int min_pitch = 21;//A0
  const int max_pitch = 108;//C8

  const int max_simultaneous_notes = 5;//we will begin with 5 that will allow us to play most of the piano scoresheets found online
  const int max_num_octaves = 9;// as in a piano keyboard

  const bool is_major = true;

  SetVarArray push;
  SetVarArray pull;
  SetVarArray playing;
  SetVarArray drt;

  //Suggestion:
  //push_harmonic
  //push_melody
  //push_countermelody
  //push_trumpet
  //and set the constraint push == push_harmonic|push_melody|push_countermelody|push_trumpet
  //this would eventually facilitate a lot the constraints especially when we have to make some constraints specific to the melody
  // SAME with pull and playing


public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pull(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    playing(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    drt(*this,bars*quantification+1,IntSet::empty,IntSet(1,bars*quantification+1)){

    // Array of 127 sets containing for each note the time where it is pushed (ot pulled)
    SetVarArray pushMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification);
    SetVarArray pullMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification);
    channel(*this, push, pushMap);
    channel(*this, pull, pullMap);

    int minLength = 24;
    int maxLength = 48;

    int scaleSize = sizeof(majorNatural)/sizeof(int);
    int progressionSize = sizeof(progression)/sizeof(int);

    rel(*this, pull[0] == IntSet::empty);
    rel(*this, push[bars*quantification] == IntSet::empty);
    rel(*this, playing[bars*quantification] == IntSet::empty);
    rel(*this, playing[0] == push[0]);
    rel(*this, drt[bars*quantification] == IntSet::empty);
    dom(*this, drt[0], SRT_SUB, minLength, maxLength);

    for(int i = 1; i <= bars*quantification; i++){
      // Notes that are playing
      rel(*this, playing[i] == ((playing[i-1] - pull[i]) | push[i]));
      // Cannot pull a note not playing
      rel(*this, pull[i] <= playing[i-1]);
      // Cannot push a note still playing
      rel(*this, push[i] || (playing[i-1] - pull[i]));
      // duration domain and cardinality
      rel(*this, cardinality(drt[i])<=cardinality(push[i]));
      if(i+maxLength <= bars*quantification+1){
    	  dom(*this, drt[i], SRT_SUB, i+minLength, i+maxLength);
      }else{
    	if(i+minLength <= bars*quantification+1){
    	  dom(*this, drt[i], SRT_SUB, i+minLength, bars*quantification+1);
    	}else{
    	  rel(*this, drt[i] == IntSet::empty);
    	}
      }
    }


    // min and max length of notes
    for(int i = 0;  i <= bars*quantification; i++){
      for (int j = 1; j < minLength && i+j <= bars*quantification ; j++){
	        rel(*this, pull[i+j] || push[i]);
      }
      SetVar z(*this, IntSet::empty, IntSet(0, max_pitch), 0, max_pitch+1);
      element(*this, SOT_UNION, pull, drt[i], z);
      rel(*this, push[i] <= z);
    }

    // For example we could indeed take part of the array using SetVarArgs like from 0 to quantification(1 measure)
    SetVarArray push_1stmeasure(*this, quantification, IntSet::empty,IntSet(0,bars*quantification+1),0,bars*quantification+1);
    for(int i=0; i<quantification; i++){
      rel(*this, push[i], SRT_EQ, push_1stmeasure[i]);
    }// and then we could channel push_1stmeasure and/or put constraints only on this part

    // If we are in C Major for example and we want our melody to oscilate around the tonic note
    rel (*this, cardinality(pushMap[62])>=10);
    // And if we want to give importance to dominant note
    rel (*this, cardinality(pushMap[69])>=5);

    // All different notes applied to the melodic part only (high pitched notes)
    for (int i=60; i<=max_pitch; i++){
      rel(*this, cardinality(pushMap[i])<=1);
    }

    //Rhythmic constraint: Fast or slow pace should also influence min length notes (and max length)
    IntVarArray cardinality_push(*this, bars*quantification+1, 0, max_simultaneous_notes);
    for(int i; i<bars*quantification+1; i++){
      rel(*this, cardinality(push[i])== cardinality_push[i]);
    }
    // Fast pace explain problem to do with union
    linear(*this, cardinality_push, IRT_GQ, 60);
    // Slow pace
    linear(*this, cardinality_push, IRT_LQ, 30);
    //Alternative way to do and probably faster, great idea but problem with disjoint union
    //SetVar allPushed;
    //rel(*this, SOT_DUNION, push, allPushed);
    //Fast pace also should also influence min length notes (and max length)
    //rel(*this, cardinality(allPushed)>=60);
    //slow pace also should also influence min length notes (and max length)
    //rel(*this, cardinality(allPushed)<=30);


    // strictly decreasing/increasing pitch
    SetVar allPlayed(*this, IntSet::empty, IntSet(0, max_pitch), 0, max_pitch+1);
    BoolVarArray isPlayed(*this, max_pitch+1, 0, 1);
    rel(*this, SOT_UNION, push, allPlayed);
    channel(*this,  isPlayed, allPlayed);
    cardinality(*this, pushMap, 0, 1);
    // strictly increasing pitch
    for (int i=60; i<=max_pitch-1; i++){
      for (int j=i+1; j<=max_pitch; j++){
	rel(*this, (isPlayed[i] && isPlayed[j]) >> (min(pushMap[i])<=min(pushMap[j])));
	// Remarks:
	// we took the min values of sets because we constrained the cardinality to be less than 1 (we cold also have taken the max value)
	// we put a <= rather than a < since we allow multiple notes to be played simoultaneously
	// unfortunately, this constraint doesn't work when there are pitches that aren't pushed :
	// the logic behind was that we constraint the pitch only if it was pushed
      }
    }
    // strictly decreasing pitch
    for (int i=60; i<=max_pitch-1; i++){
      for (int j=i+1; j<=max_pitch; j++){
	rel(*this, (isPlayed[i] && isPlayed[j]) >> (min(pushMap[i])>=min(pushMap[j])));
	// Remarks:
	// we took the min values of sets because we constrained the cardinality to be less than 1 (we cold also have taken the max value)
	// we put a >= rather than a > since we allow multiple notes to be played simoultaneously
	// unfortunately, this constraint doesn't work when there are pitches that aren't pushed :
	// the logic behind was that we constraint the pitch only if it was pushed
      }
    }



    //constraint that clement asked
    /*BoolVarArray timePlayed(*this,bars*quantification+1, 0, 1);
    SetVar unionPush(*this, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification+1);
    rel(*this, SOT_UNION, pushMap, unionPush);
    channel(*this, timePlayed, unionPush);
    float percentPlayed=0.5;
    linear(*this, timePlayed, IRT_EQ,(bars*quantification-1)*percentPlayed+1);*/

    float percentPlayed=1.0;
    if (percentPlayed == 0.0){
      SetVar unionPush(*this, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification+1);
      rel(*this, SOT_UNION, pushMap, unionPush);
      cardinality(*this, unionPush, 1, 1);
    }else{
      int pushEvery = int(minLength/percentPlayed);
      for(int i=0;  i<bars*quantification; i++){
	if(i%pushEvery==0){
	  //cardinality(*this, push[i], IRT_GQ, 1);
	  rel(*this, cardinality(push[i])>=1);
	}else{
	  cardinality(*this, push[i], IRT_EQ, 0);
	}
      }
    }


    // increasing/decreasing pitch // TODO re check, something is going wrong
    SetVar allPlayed(*this, IntSet::empty, IntSet(0, max_pitch), 0, max_pitch+1);
    BoolVarArray isPlayed(*this, max_pitch+1, 0, 1);
    rel(*this, SOT_UNION, push, allPlayed);
    channel(*this,  isPlayed, allPlayed);
    // increasing pitch
    for (int i=60; i<=max_pitch-1; i++){
      for (int j=i+1; j<=max_pitch; j++){
	rel(*this, (isPlayed[i] && isPlayed[j]) >> (max(pushMap[i])<=min(pushMap[j])));
	// Remarks:
	// we put a <= rather than a < since we allow multiple notes to be played simoultaneously
	// unfortunately, this constraint doesn't work when there are pitches that aren't pushed :
	// the logic behind was that we constraint the pitch only if it was pushed
      }
    }
    //decreasing pitch
    for (int i=60; i<=max_pitch-1; i++){
      for (int j=i+1; j<=max_pitch; j++){
	rel(*this, (isPlayed[i] && isPlayed[j]) >> (max(pushMap[i])>=min(pushMap[j])));
	// Remarks:
	// we put a >= rather than a > since we allow multiple notes to be played simoultaneously
	// unfortunately, this constraint doesn't work when there are pitches that aren't pushed :
	// the logic behind was that we constraint the pitch only if it was pushed
      }
    }

    // TODO ADD the intervals as in damien
    IntVarArgs max_push;
    IntVarArgs min_push;
    //first << zero;
    //first<<IntVar(*this, 0, 0);
    for(int i=0; i<=bars*quantification; i++){
      rel(*this, (push[i] != IntSet::empty) >> (max_push << IntVar(*this, max(push[i]),  max(push[i]))) );//check si ca fonctionne
      rel(*this, (push[i] != IntSet::empty) >> (min_push << IntVar(*this, min(push[i]),  min(push[i]))) );
    }
    //mostly increasing
    IntVarArgs most_inc;
    rel(*this, most_inc == max_push.slice(0,1,max_push.size()-2) - min_push.slice(1,1,min_push.size()-2));//check si ca fonctionne
    IntVar inc ;
    count(*this, most_inc, 0, IRT_GR, inc );
    rel(*this, inc  >= 0.8*max_push.size());
    //mostly decreasing
    IntVarArgs most_dec;
    rel(*this, most_dec == min_push.slice(0,1,min_push.size()-2) -  max_push.slice(0,1,max_push.size()-2));
    IntVar dec ;
    count(*this, most_dec, 0, IRT_GR, dec);
    rel(*this, dec >= 0.8*max_push.size());


    /*/max length
    int maxLength = 60;
    for(int i=0; i<max_pitch; i++){
      IntVarArgs pushTmp;
      IntVarArgs pullTmp;
      channelSorted(home, pushTmp, pushMap[i]);
      channelSorted(home, pullTmp, pullMap[i]);
      IntVarArgs diff;
      rel(*this, diff == pullTmp - pushTmp);
      rel(*this, diff<=maxLength);
      }*/


    cardinality(*this, playing, 0, max_simultaneous_notes); //We should maybe do it in the constructor in order to be more performant
    cardinality(*this, pull, 0, max_simultaneous_notes);
    cardinality(*this, push, 0, max_simultaneous_notes);


    // Create scale
    int scale[7];
    int key = 0;
    for(int i=1; i<scaleSize; i++){
      scale[i] = key%12;
      if(is_major)
	key += majorNatural[i];
    }

    // Following a scale

    std::vector<int> v;
    for (int octave = 0; octave < 8; octave++){
      for (int i = 0; i<scaleSize; i++){
	v.push_back(octave*12+scale[i]);
      }
    }

    IntArgs a(v);
    IntSet scaleSet(a);

    for(int i = 0; i < bars*quantification; i++){
      rel(*this, push[i] <= scaleSet);
    }

    /*  Following a chord progression
	for (int i = 0; i < progressionSize; i++){
        std::vector<int> v;
        for (int octave = 0; octave < 11; octave++){
	v.push_back(octave*12 + scale[progression[i]-1]);
	v.push_back(octave*12 + scale[(progression[i]+1)%scaleSize]);
	v.push_back(octave*12 + scale[(progression[i]+3)%scaleSize]);
        }

        IntArgs a(v);
        IntSet chordSet(a);
        for(int j = 0; j < bars*quantification/progressionSize; j++){//this forloop will constrain the push to be a subset of the chord progression for the whole measure
	  //since here we chose a quantification of 48 and a min_length of 24 we will have chord 1 twice, 5 twice ....
          rel(*this, push[j + i*bars*quantification/progressionSize] <= chordSet);// push subset of chordset
        }
	}//*/


    //If we know the chord progression and the chord rhythm, it would make sense to put them togehter if the composer wants to do 1 5 6 4 and then 1 5 6 4 again
    for (int i = 0; i < chords_per_bar*bars; i++){
      std::vector<int> v;
      for (int octave = 0; octave < 8; octave++){
	v.push_back(octave*12 + scale[progression[i%progressionSize]-1]);
	v.push_back(octave*12 + scale[(progression[i%progressionSize]+1)%scaleSize]);
	v.push_back(octave*12 + scale[(progression[i%progressionSize]+3)%scaleSize]);
      }
      IntArgs a(v);
      IntSet chordSet(a);
      rel(*this, push[i*quantification/chords_per_bar] <= chordSet);
    }

    // Constraining chord rhythm
    for (int i = 0; i < bars*quantification; i++){
      if (i % (quantification/chords_per_bar) == 0){
	cardinality(*this, push[i], 3, 5);
        } else {
	cardinality(*this, push[i], 0, 1);
      }
    }


    // Limiting the range of notes
    dom(*this, push, SRT_SUB, 2*12, 4*12);

    /*/ Constraining the min length of the notes
    int minlength = 24;
    for (int i = 0; i <= bars*quantification; i++){
      for (int j = 1; j < minlength && i+j <= bars*quantification ; j++){
	rel(*this, pull[i+j] || push[i]);
      }
      }*/


    // TO DO Test with different r1 and r2 and see results
    for (int i = 0; i <= bars*quantification; i++){
      //Rnd r1(opt.seed());
      //r1.time();
      Rnd r2(opt.seed());
      r2.time();
      branch(*this, push[i], SET_VAL_RND_INC(r2));
      branch(*this, pull[i], SET_VAL_RND_INC(r2));
    }



  }

  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody){

      push.update(*this, melody.push);
      pull.update(*this, melody.pull);
      playing.update(*this, melody.playing);

  }

  /// Copy during cloning
  virtual Space*
  copy(void) {
    return new Melody(*this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {

    os << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(push[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(pull[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(playing[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl;
  }
};

/** \brief Main-function
 *  \relates melody
 */
int main(int argc, char* argv[]) {
  SizeOptions opt("Melody");
  opt.solutions(1);
  opt.parse(argc,argv);
  Script::run<Melody,DFS,SizeOptions>(opt);
  return 0;
}

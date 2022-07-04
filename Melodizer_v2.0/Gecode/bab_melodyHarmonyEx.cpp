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

  const int bars = 2;
  const int quantification = 8;
  
  const int chord_notes = 3;// to begin with we will concentrate in chords of three notes, seventh chords will come later
  //const int key = 21;// from 21(A0) to 108(C8)

  const int chords_per_bar = 2;
  
  const int max_pitch = 108;//C8

  const int max_simultaneous_notes = 5;//we will begin with 5 that will allow us to play most of the piano scoresheets found online
  const int max_num_octaves = 9;// as in a piano keyboard

  const bool is_major = true;

  int minLengthMelodic = 2;
  int minLengthHarmonic = 4;

  SetVarArray pushMelodic;
  SetVarArray pushHarmonic;
  SetVarArray pullMelodic;
  SetVarArray pullHarmonic;

  SetVar pushHarmonicAll;
  
  SetVarArray push;
  SetVarArray pull;
  SetVarArray playing;

  Rnd r1;
  //r1.time();
  Rnd r2;
  //r2.time();
  Rnd r3;
  //r3.time();
  Rnd r4;
  //r4.time();

public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pull(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    playing(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pushMelodic(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pullMelodic(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pushHarmonic(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pullHarmonic(*this,bars*quantification+1,IntSet::empty,IntSet(0,max_pitch)),
    pushHarmonicAll(*this,IntSet::empty, IntSet(0,max_pitch), 0,max_pitch),r1(opt.seed()),r2(opt.seed()),r3(opt.seed()),r4(opt.seed())
  {

    // Array of 127 sets containing for each note the time where it is pushed (ot pulled)
    SetVarArray pushMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification);
    SetVarArray pullMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification),0,bars*quantification);
    channel(*this, pushMelodic, pushMap);
    channel(*this, pullMelodic, pullMap);
	    
    
    int scaleSize = sizeof(majorNatural)/sizeof(int);
    int progressionSize = sizeof(progression)/sizeof(int);

    rel(*this, pullMelodic[0] == IntSet::empty);
    rel(*this, pullHarmonic[0] == IntSet::empty);
    rel(*this, pull[0] == IntSet::empty);
    rel(*this, push[bars*quantification] == IntSet::empty);
    rel(*this, pushMelodic[bars*quantification] == IntSet::empty);
    rel(*this, pushHarmonic[bars*quantification] == IntSet::empty);
    rel(*this, playing[bars*quantification] == IntSet::empty);
    rel(*this, playing[0] == push[0]);
    rel(*this, push[0] == (pushHarmonic[0] | pushMelodic[0]));
    rel(*this, pull[0] == (pullHarmonic[0] | pullMelodic[0]));
    rel(*this, IntSet::empty == (pushHarmonic[0] & pushMelodic[0]));
    rel(*this, IntSet::empty == (pullHarmonic[0] & pullMelodic[0]));

    for(int i = 1; i <= bars*quantification; i++){
      // Notes that are playing
      rel(*this, playing[i] == ((playing[i-1] - pull[i]) | push[i])); 
      // Cannot pull a note not playing
      rel(*this, pull[i] <= playing[i-1]);
      // Cannot push a note still playing
      rel(*this, push[i] || (playing[i-1] - pull[i]));
      //push/pull is the union of push/pullHarmonic and push/pullMelodic
      rel(*this, push[i] == (pushHarmonic[i] | pushMelodic[i]));
      rel(*this, pull[i] == (pullHarmonic[i] | pullMelodic[i]));
      rel(*this, IntSet::empty == (pushHarmonic[i] & pushMelodic[i]));
      rel(*this, IntSet::empty == (pullHarmonic[i] & pullMelodic[i]));
    }

    
    // If we are in C Major for example and we want our melody to oscilate around the tonic note 
    rel (*this, cardinality(pushMap[62])>=4);
    // And if we want to give importance to dominant note
    rel (*this, cardinality(pushMap[69])>=2);
    
    
    cardinality(*this, playing, 0, max_simultaneous_notes); //We should maybe do it in the constructor in order to be more performant
    cardinality(*this, pull, 0, max_simultaneous_notes);
    cardinality(*this, push, 0, max_simultaneous_notes);


    cardinality(*this, pullHarmonic, 0, 3);
    cardinality(*this, pushHarmonic, 0, 3);
    cardinality(*this, pullMelodic, 0, 1);
    cardinality(*this, pushMelodic, 0, 1);
    
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
      rel(*this, pushHarmonic[i*quantification/chords_per_bar] <= chordSet);
    }

    
    // Constraining chord rhythm
    for (int i = 0; i < bars*quantification; i++){
      if (i % (quantification/chords_per_bar) == 0){
	cardinality(*this, pushHarmonic[i], 3, 3);
        } else {
	cardinality(*this, pushHarmonic[i], 0, 0);
      }
    }
    
    
    // Limiting the range of notes
    dom(*this, pushMelodic, SRT_SUB, 60, 81);
    dom(*this, pushHarmonic, SRT_SUB, 40, 64);
      
    // Constraining the min length of the notes
    for (int i = 0; i <= bars*quantification; i++){
      for (int j = 1; j < minLengthMelodic && i+j <= bars*quantification ; j++){
	rel(*this, pullMelodic[i+j] || pushMelodic[i]);
      }
    }
    for (int i = 0; i <= bars*quantification; i++){
      for (int j = 1; j < minLengthHarmonic && i+j <= bars*quantification ; j++){
	rel(*this, pullHarmonic[i+j] || pushHarmonic[i]);
      }
    }

    
    rel(*this, SOT_UNION, pushHarmonic, pushHarmonicAll);
    //rel(*this, max(pushHarmonicAll)==57);
    
    
    // TO DO Test with different r1 and r2 and see results
    for (int i = 0; i <= bars*quantification; i++){
      branch(*this, pushHarmonic[i], SET_VAL_RND_INC(r1));
      branch(*this, pushMelodic[i], SET_VAL_RND_INC(r2));
      branch(*this, pullHarmonic[i], SET_VAL_RND_INC(r3));
      branch(*this, pullMelodic[i], SET_VAL_RND_INC(r4));
    }
    
    

  }

  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody), r1(melody.r1),  r2(melody.r2), r3(melody.r3), r4(melody.r4){
    pushMelodic.update(*this, melody.pushMelodic);
    pullMelodic.update(*this, melody.pullMelodic);
    pushHarmonic.update(*this, melody.pushHarmonic);
    pullHarmonic.update(*this, melody.pullHarmonic);
    push.update(*this, melody.push);
    pull.update(*this, melody.pull);
    playing.update(*this, melody.playing);
    pushHarmonicAll.update(*this, melody.pushHarmonicAll);

  }

  /// Copy during cloning
  virtual Space*
  copy(void) {
    return new Melody(*this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {

    // os << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(push[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    /*for (int i = 0; i<=bars*quantification; i++) {
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
    os << std::endl;/**/

    //os << max(pushHarmonicAll) >> min(pushHarmonicAll) >> std::endl;
  }

  virtual void constrain(const Space& _b) {
    const Melody& b = static_cast<const Melody&>(_b);
    SetVar tmp(b.pushHarmonicAll);
    rel(*this, max(pushHarmonicAll)-min(pushHarmonicAll) < max(tmp)-min(tmp) );
  }
};

/** \brief Main-function
 *  \relates melody
 */
int main(int argc, char* argv[]) {
  SizeOptions opt("Melody");
  opt.solutions(7);
  opt.parse(argc,argv);
  Script::run<Melody,BAB,SizeOptions>(opt);
  return 0;
}

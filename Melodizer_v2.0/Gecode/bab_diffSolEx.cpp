#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/int/arithmetic.hh>
#include <gecode/set.hh>
#include <vector>
#include <iostream>
#include <set>
#include <string>
#include <algorithm>
#include <cstdlib>
#include <ctime>

using namespace Gecode;

namespace {
  int majorNatural[7] = {2, 2, 1, 2, 2, 2, 1};
  int minorNatural[7] = {2, 1, 2, 2, 2, 1, 2};

  int progression[4] = {1, 5, 6, 4};
  int numDifFingers = 3;
};

class Melody : public Script {
private:

  const int bars = 2;
  const int quantification = 4;

  SetVarArray push;
  SetVarArray pull;
  SetVarArray playing;
  //SetVarArray drt;

  // BoolVarArray isPlayed;
  //SetVarArray pushMap;
  //SetVarArray pullMap;
  int minLength = 2;

  
  int percent_diff = 50;

  const int max_pitch = 5;

public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification+1,IntSet::empty,IntSet(0,5)),
    pull(*this,bars*quantification+1,IntSet::empty,IntSet(0,5)),
    playing(*this,bars*quantification+1,IntSet::empty,IntSet(0,5))//,
    //drt(*this,bars*quantification+1,IntSet::empty,IntSet(1,4))
    //pushMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification)),
    //pullMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification))
  {

    
    int maxLength = 3;


    SetVarArray pushMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification));
    SetVarArray pullMap(*this, max_pitch+1, IntSet::empty,IntSet(0,bars*quantification));
    channel(*this, push, pushMap);
    channel(*this, pull, pullMap);  

    rel(*this, pull[0] == IntSet::empty);
    rel(*this, push[bars*quantification] == IntSet::empty);
    rel(*this, playing[bars*quantification] == IntSet::empty);
    //rel(*this, drt[bars*quantification] == IntSet::empty);
    rel(*this, playing[0] == push[0]);
    //dom(*this, drt[0], SRT_SUB, minLength, maxLength);
    
    for(int i = 1; i <= bars*quantification; i++){
      // Notes that are playing
      rel(*this, playing[i] == ((playing[i-1] - pull[i]) | push[i])); 
      // Cannot pull a note not playing
      rel(*this, pull[i] <= playing[i-1]);
      // Cannot push a note still playing
      rel(*this, push[i] || (playing[i-1] - pull[i]));
      // duration domain and cardinality
      /*rel(*this, cardinality(drt[i])<=cardinality(push[i]));
      if(i+maxLength <= bars*quantification+1){
	dom(*this, drt[i], SRT_SUB, i+minLength, i+maxLength);
      }else{
	if(i+minLength <= bars*quantification+1){
	  dom(*this, drt[i], SRT_SUB, i+minLength, bars*quantification+1);
	}else{
	  rel(*this, drt[i] == IntSet::empty);
	}
	}*/
    }

    /*/ min and max length of notes
    for(int i = 0;  i <= bars*quantification; i++){
      for (int j = 1; j < minLength && i+j <= bars*quantification ; j++){
	rel(*this, pull[i+j] || push[i]);
      }
      SetVar z(*this, IntSet::empty, IntSet(0, max_pitch), 0, max_pitch+1);
      element(*this, SOT_UNION, pull, drt[i], z);
      rel(*this, push[i] <= z);
      }*/
    
    cardinality(*this, playing, 0, 3); //We should maybe do it in the constructor in order to be more performant
    cardinality(*this, pull, 0, 4);
    cardinality(*this, push, 0, 4);

    for(int i = 0;  i <= bars*quantification; i++){
      for (int j = 1; j < minLength && i+j <= bars*quantification ; j++){
	rel(*this, pull[i+j] || push[i]);
      }}

    
    std::vector<int> v;
    v.push_back(4);
    IntArgs a(v);
    IntSet set0(a);
    std::vector<int> b;
    v.push_back(3);
    IntArgs c(b);
    IntSet set1(c);

    //rel(*this, push[0] == set0);
    //rel(*this, push[1] == set1);
    //rel(*this, pull[2] == IntSet::empty);
    //rel(*this, pull[3] == IntSet::empty);
    //rel(*this, pull[4] == set0);
    //rel(*this, max(push[1])<=3);
    //rel(*this, min(push[2])>max(push[1]));
    //rel(*this, (push[4]==IntSet::empty)  >> (min(push[3])>max(push[4])) );//doesn't work
    //rel(*this, ((push[3]!=)&&()) >> );
    
    
    Rnd r1(opt.seed());
    r1.time();
    Rnd r2(opt.seed());
    r2.time();
    
    for (int i = 0; i <= bars*quantification; i++){//put different random number in each iteration
      branch(*this, push[i], SET_VAL_RND_INC(r2));
      branch(*this, pull[i], SET_VAL_RND_EXC(r1));
      //branch(*this, drt[i], SET_VAL_RND_EXC(r2));
      }

    //branch(*this, pushMap, SET_VAR_NONE(), SET_VAL_RND_INC(r2));
    //branch(*this, pullMap, SET_VAR_NONE(), SET_VAL_RND_INC(r2));

    //branch(*this, push, SET_VAR_NONE());
    //branch(*this, pull, SET_VAR_NONE());
    
  }
  
  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody){

      push.update(*this, melody.push);
      pull.update(*this, melody.pull);
      playing.update(*this, melody.playing);
      //drt.update(*this, melody.drt);
      //pushMap.update(*this, melody.pushMap);
      //pullMap.update(*this, melody.pullMap);
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
    os << std::endl << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(push[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
  }

  virtual void constrain(const Space& _b) {
    const Melody& b = static_cast<const Melody&>(_b);
    for(int i=0; i<=bars*quantification-minLength; i=i+1){//-2 car c'est le minLenght
      if((rand()%100)< percent_diff){
	SetVar tmp(b.push[i]);
	rel(*this, push[i] != tmp);
      }
      //rel(*this, (tmp!=IntSet::empty) >> (push[i] != tmp) ); si on veut que ca change que quand tmp n'est pas nul
    }
      //rel(this, cardinality(b.push)>cardinality(push));
  }
  
};

/** \brief Main-function
 *  \relates melody
 */
int main(int argc, char* argv[]) {
  srand(time(NULL));
  SizeOptions opt("Melody");
  std::cout << rand() << std::endl;
  //std::cout << rand() << std::endl;
  opt.solutions(4);
  opt.parse(argc,argv);
  Script::run<Melody,BAB,SizeOptions>(opt);
  return 0;
}

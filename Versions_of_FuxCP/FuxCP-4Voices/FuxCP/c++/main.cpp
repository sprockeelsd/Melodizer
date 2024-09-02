// 
// Created by Luc Cleenewerk and Diego de Patoul. 
//

#include "headers/Utilities.hpp"
#include "headers/CounterpointUtils.hpp"
#include "headers/Parts/Part.hpp"
#include "headers/Parts/FirstSpeciesCounterpoint.hpp"
#include "headers/Parts/SecondSpeciesCounterpoint.hpp"
#include "headers/Parts/CantusFirmus.hpp"
#include "headers/CounterpointProblems/TwoVoiceCounterpoint.hpp"
#include "headers/CounterpointProblems/CounterpointProblem.hpp"
#include "headers/fuxTest.hpp"

using namespace Gecode;
using namespace std;

int main(int argc, char* argv[]) {
    if(argc==1){
        cout << argv[0] << endl;
        cout << "-------------" << endl;
        vector<Species> species = {THIRD_SPECIES};
        //la do si re do mi fa mi re do si la
        //57 60 59 62 60 64 65 64 62 60 59 57
        // vector<int> cantusFirmus = {57,60,59,62,60,64,65,64,62,60,59,57}; //1sp 2v cf
        vector<int> cantusFirmus = {60,   62,   65,   64,   67,   65,   64,   62,   60};
        //cantusFirmus = {62,   65,   64,   62,   67,   65,   69,   67,   65, 64, 62};
        
        int size = cantusFirmus.size();
        vector<int> v_type = {2 , 1, -1};

        vector<int> melodic_params = {0, 1, 1, 576, 2, 2, 2, 1};
        //borrow, h-5th, h-octave, succ, variety, triad, direct move, penult rule check
        vector<int> general_params = {4, 1, 1, 2, 2, 2, 8, 1};

        //penult sixth, non-ciambata, con m after skip, h triad 3rd species, m2 eq zero, no syncopation, pref species slider
        vector<int> specific_params = {8 , 4 , 0 , 2 , 1 , 8 , 50};


        vector<int> importance = {8,7,5,2,9,3,14,12,6,11,4,10,1,13};

        int borrowMode = 1;

        // create a new problem
        // auto* problem = new TwoVoiceCounterpoint(cantusFirmus, species[0], C, lower_bound_domain, upper_bound_domain);
        auto* problem = create_problem(cantusFirmus, species, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
        
        //solution from illustration 8.4
        //vector<int> sol = {76,77,79,77,74,76,83,72,74,72,74,72,79,72,74,76,81,77,79,83,81,76,77,79,81,74,76,83,81,79,77,79,76,67,65,62,60,60,62,60,65,67,48,50,50,48,48,50,48,50,48};
        //for(int i = 0; i < problem->getSize(); i++){
        //    rel(problem->getHome(), problem->getSolutionArray()[i], IRT_EQ, sol[i]);
        //}
        //solution from illustration 8.2
        //costs : ...,...,octave,...,fifth,...,...,melodic,motion
        //vector<int> sol = {88,81,81,83,83,84,72,74,79,67,65,60,67,62,69,69,69,64,48,50,53,52,55,53,57,65,60};
        //for(int i = 0; i < problem->getSize(); i++){
        //    rel(problem->getHome(), problem->getSolutionArray()[i], IRT_EQ, sol[i]);
        //}

        //solution from illustration 8.3
        //costs : ...,...,octave,...,fifth,...,...,melodic,motion
        //vector<int> sol = {76,81,74,86,83,86,79,88,81,88,86,89,81,79,89,86,79,67,65,62,60,64,69,69,69,64,48,50,50,48,48,50,48,50,48};
        //for(int i = 0; i < problem->getSize(); i++){
        //    rel(problem->getHome(), problem->getSolutionArray()[i], IRT_EQ, sol[i]);
        //}

        //solution from illustration 8.5
        //costs : ...,...,octave,...,fifth,...,...,melodic,motion
        //vector<int> sol = {72,72,76,76,81,81,74,74,76,76,83,83,79,79,77,72,60,65,62,60,64,62,69,69,64,48,50,50,48,48,50,48,50,48};
        //for(int i = 0; i < problem->getSize(); i++){
        //    rel(problem->getHome(), problem->getSolutionArray()[i], IRT_EQ, sol[i]);
        //}

        /*
        rel(problem->getHome(), problem->getSolutionArray()[0], IRT_EQ, 57);
        rel(problem->getHome(), problem->getSolutionArray()[1], IRT_EQ, 57);
        rel(problem->getHome(), problem->getSolutionArray()[2], IRT_EQ, 60);
        rel(problem->getHome(), problem->getSolutionArray()[3], IRT_EQ, 59);
        rel(problem->getHome(), problem->getSolutionArray()[4], IRT_EQ, 59);

        rel(problem->getHome(), problem->getSolutionArray()[11], IRT_EQ, 53);
        rel(problem->getHome(), problem->getSolutionArray()[12], IRT_EQ, 53);
        rel(problem->getHome(), problem->getSolutionArray()[13], IRT_EQ, 55);
        rel(problem->getHome(), problem->getSolutionArray()[14], IRT_EQ, 55);
        rel(problem->getHome(), problem->getSolutionArray()[15], IRT_EQ, 55);

        rel(problem->getHome(), problem->getSolutionArray()[22], IRT_EQ, 50);
        rel(problem->getHome(), problem->getSolutionArray()[23], IRT_EQ, 50);
        rel(problem->getHome(), problem->getSolutionArray()[24], IRT_EQ, 48);
        rel(problem->getHome(), problem->getSolutionArray()[25], IRT_EQ, 55);
        rel(problem->getHome(), problem->getSolutionArray()[16], IRT_EQ, 52);*/
        Search::Base<CounterpointProblem>* e = make_solver(problem, bab_solver);
        int nb_sol = 0;
        while(CounterpointProblem* pb = get_next_solution_space(e)){
            
            nb_sol++;
            cout << "Solution " << nb_sol << ": " << endl;
            cout << pb->to_string() << endl;
            cout << pb->getSize() << endl;
            // cout << int_vector_to_string(cantusFirmus) << endl;

            delete pb;
            if (nb_sol >= 1)
                break;
        }
        cout << "No (more) solutions." << endl;
    } else if(argc==3){
        FuxTest* test = new FuxTest(atoi(argv[1]), atoi(argv[2]));
        vector<Species> species = test->getSpList();
        //la do si re do mi fa mi re do si la
        //57 60 59 62 60 64 65 64 62 60 59 57
        vector<int> cantusFirmus = test->getCf();
        
        int size = cantusFirmus.size();
        vector<int> v_type = test->getVType();

        vector<int> melodic_params = {0, 1, 1, 576, 2, 2, 2, 1};
        //borrow, h-5th, h-octave, succ, variety, triad, direct move, penult rule check
        vector<int> general_params = {4, 1, 1, 2, 2, 2, 8, 1};

        //penult sixth, non-ciambata, con m after skip, h triad 3rd species, m2 eq zero, no syncopation, pref species slider
        vector<int> specific_params = {8 , 4 , 0 , 2 , 1 , 8 , 50};

        vector<int> importance = {8,7,5,2,9,3,14,12,6,11,4,10,1,13};

        int borrowMode = test->getBMode();

        // create a new problem
        // auto* problem = new TwoVoiceCounterpoint(cantusFirmus, species[0], C, lower_bound_domain, upper_bound_domain);
        auto* problem = create_problem(cantusFirmus, species, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
        //cout << problem->to_string() << endl;
        // create a new search engine

        if(atoi(argv[1])==125){
            for(int j = 0; j < problem->getSize(); j++){
                if(j!=10){
                    rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                }
            }
        }
        else if(atoi(argv[1])==74){
            for(int j = 0; j < test->getIdx(); j++){
                if(j!=0){
                    rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                }
            }
        }
        else if(atoi(argv[1])==82){
            vector<int> speciesArray = {-1,-1,3,-1,3,2,2,2,2,2,2,2,2,2,3,-1,3,2,2,2,2,2,3,-1,3,-1,3,-1,3,2,3,-1,3,2,3,-1,3,-1,3,-1,3};
            for(int j = 0; j < test->getIdx(); j++){
                if(j>1){
                    rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                    //rel(problem->getHome(), problem->get_species_array_5sp(0)[j], IRT_EQ, test->getCp()[j]);
                }
            }
        }
        else{
            if(species[0]==FIRST_SPECIES || species[0]==THIRD_SPECIES || species[0]==FOURTH_SPECIES){
                for(int j = 0; j < test->getIdx(); j++){
                    rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                }
            } else if(species[0]==SECOND_SPECIES){
                for(int j = 1; j < test->getIdx(); j++){
                    rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                }
            }
        }

        BAB<CounterpointProblem> e(problem);

        int nb_sol = 0;
        while(CounterpointProblem* pb = e.next()){
            nb_sol++;
            cout << "Solution " << nb_sol << ": " << endl;
            cout << pb->to_string() << endl;
            // cout << int_vector_to_string(cantusFirmus) << endl;

            delete pb;
            if (nb_sol >= 1)
                break;
        }
        cout << "No (more) solutions." << endl;
    } else if(argc==2){
        char* str = argv[1];
        if(!notInt(str)){
            FuxTest* test = new FuxTest(atoi(argv[1]));
            vector<Species> species = test->getSpList();
            //la do si re do mi fa mi re do si la
            //57 60 59 62 60 64 65 64 62 60 59 57
            vector<int> cantusFirmus = test->getCf();
            
            int size = cantusFirmus.size();
            vector<int> v_type = test->getVType();

            vector<int> melodic_params = {0, 1, 1, 576, 2, 2, 2, 1};
            //borrow, h-5th, h-octave, succ, variety, triad, direct move, penult rule check
            vector<int> general_params = {4, 1, 1, 2, 2, 2, 8, 1};

            //penult sixth, non-ciambata, con m after skip, h triad 3rd species, m2 eq zero, no syncopation, pref species slider
            vector<int> specific_params = {8 , 4 , 0 , 2 , 1 , 8 , 50};

            vector<int> importance = {8,7,5,2,9,3,14,12,6,11,4,10,1,13};

            int borrowMode = test->getBMode();

            // create a new problem
            // auto* problem = new TwoVoiceCounterpoint(cantusFirmus, species[0], C, lower_bound_domain, upper_bound_domain);
            auto* problem = create_problem(cantusFirmus, species, v_type, melodic_params, general_params, specific_params,
                importance, borrowMode);
            //cout << problem->to_string() << endl;
            // create a new search engine
            if(atoi(argv[1])==125){
                cout << "HERE" << endl;
                for(int j = 0; j < problem->getSize(); j++){
                    if(j!=10){
                        rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                    }
                }
            }
            else{
                if(species[0]==FIRST_SPECIES || species[0]==THIRD_SPECIES){
                    for(int j = 0; j < problem->getSize(); j++){
                        rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                    }
                } else if(species[0]==SECOND_SPECIES || species[0]==FOURTH_SPECIES){
                    for(int j = 1; j < problem->getSize(); j++){
                        rel(problem->getHome(), problem->getSolutionArray()[j], IRT_EQ, test->getCp()[j]);
                    }
                }
            }

            BAB<CounterpointProblem> e(problem);

            int nb_sol = 0;
            while(CounterpointProblem* pb = e.next()){
                nb_sol++;
                
                // cout << int_vector_to_string(cantusFirmus) << endl;
                cout << "Solution " << nb_sol << ": " << endl;
                cout << pb->to_string() << endl;
                delete pb;
                if (nb_sol >= 1)
                    break;
            }
            if(nb_sol!=1){
                cout << "This test did not pass. An error was found!" << endl;
            } else {
                cout << "This test passed successfully!" << endl;
            }
        } else {
            FuxTest* test = new FuxTest(argv[1]);
        }
    } else {
        throw std::invalid_argument("Wrong amount of arguments!");
    }
    
    return 0;
}
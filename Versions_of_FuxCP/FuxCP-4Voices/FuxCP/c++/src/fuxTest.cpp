// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the testing framework implementation.  
//

#include "../headers/fuxTest.hpp"

FuxTest::FuxTest(char* test){
    
    cantusFirmus = {60,   62,   65,   64,   67,   65,   64,   62,   60};
    size = cantusFirmus.size();
    melodic_params = {0, 1, 1, 576, 2, 2, 2, 1};
    general_params = {4, 1, 1, 2, 2, 2, 8, 1};
    specific_params = {8 , 4 , 0 , 2 , 1 , 8 , 50};
    importance = {8,7,5,2,9,3,14,12,6,11,4,10,1,13};
    borrowMode = 1;
    CounterpointProblem* problem;
    problem = dispatcher(test);
    BAB<CounterpointProblem> e(problem);
    int nb_sol = 0;
    while(CounterpointProblem* pb = e.next()){
        nb_sol++;
        cout << pb->to_string() << endl;
        delete pb;
        if (nb_sol >= 1)
            break;
    }
    if(nb_sol==0){
        cout << "This constraint works. It prohibits a forbidden configuration." << endl;
    } else {
        cout << "This constraint does NOT work. It allows a forbidden configuration." << endl;
    }

}

CounterpointProblem* FuxTest::dispatcher(char* test){
    if(strcmp(test, "1H1")==0){
        return test_1sp_H1();
    } else if(strcmp(test, "1H2")==0){
        return test_1sp_H2();
    } else if(strcmp(test, "1H32")==0){
        return test_1sp_H3();
    } else if(strcmp(test, "1H33")==0){
        return test_1sp_H3_2();
    } else if(strcmp(test, "1H41")==0){
        return test_1sp_H4_1();
    } else if(strcmp(test, "1H42")==0){
        return test_1sp_H4_2();
    } else if(strcmp(test, "1H5")==0){
        return test_1sp_H5();
    } else if(strcmp(test, "1H7")==0){
        return test_1sp_H7();
    } else if(strcmp(test, "1H72")==0){
        return test_1sp_H7_2();
    } else if(strcmp(test, "2H2")==0){
        return test_2sp_H2();
    } else if(strcmp(test, "3H1")==0){
        return test_3sp_H1();
    } else if(strcmp(test, "4H2")==0){
        return test_4sp_H2();
    } else {
        std::invalid_argument("Test for constraint not found!");
        return nullptr;
    }
}

FuxTest::FuxTest(int testNumber): FuxTest(testNumber, 0){
    idx = cp.size();
}

FuxTest::FuxTest(int testNumber, int i){
    if(testNumber==22){
        test_2v_1sp_fig22_setter(i);
    } else if(testNumber==23){
        test_2v_1sp_fig23_setter(i);
    } else if(testNumber==38){
        test_2v_2sp_fig38_setter(i);
    } else if(testNumber==39){
        test_2v_2sp_fig39_setter(i);
    } else if(testNumber==40){
        test_2v_2sp_fig40_setter(i);
    } else if(testNumber==55){
        test_2v_3sp_fig55_setter(i);
    } else if(testNumber==74){
        test_2v_4sp_fig74_setter(i);
    }else if(testNumber==82){
        test_2v_5sp_fig82_setter(i);
    }else if(testNumber==108){
        test_3v_1sp_fig108_setter(i);
    } else if(testNumber==109){
        test_3v_1sp_fig109_setter(i);
    } else if(testNumber==110){
        test_3v_1sp_fig110_setter(i);
    } else if(testNumber==111){
        test_3v_1sp_fig111_setter(i);
    } else if(testNumber==125){
        test_3v_2sp_fig125_setter(i);
    } else if(testNumber==146){
        test_3v_4sp_fig146_setter(i);
    } else if(testNumber==166){
        test_4v_1sp_fig166_setter(i);
    } else if(testNumber==167){
        test_4v_1sp_fig167_setter(i);
    } else if(testNumber==176){
        test_4v_2sp_fig176_setter(i);
    }
    else {
        throw std::invalid_argument("This test number has not been implemented (yet)");
    }
}

CounterpointProblem* FuxTest::test_1sp_H1(){
    spList = {FIRST_SPECIES};
    v_type = {2};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[1], IRT_EQ, 72); //does not work since it is not a consonant
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H2(){
    spList = {FIRST_SPECIES};
    v_type = {2};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[0], IRT_EQ, 64); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H3(){
    spList = {FIRST_SPECIES};
    v_type = {2};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[problem->getSize()-1], IRT_EQ, 64); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H3_2(){
    spList = {FIRST_SPECIES, FIRST_SPECIES};
    v_type = {2, 1};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[(problem->getSize()/2)-1], IRT_EQ, 67); 
    rel(problem->getHome(), problem->getSolutionArray()[(problem->getSize())-1], IRT_EQ, 65); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H4_1(){
    spList = {FIRST_SPECIES};
    v_type = {1};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[0], IRT_EQ, 67); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H4_2(){
    spList = {FIRST_SPECIES};
    v_type = {1};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[problem->getSize()-1], IRT_EQ, 67); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H5(){
    spList = {FIRST_SPECIES};
    v_type = {0};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[1], IRT_EQ, 62); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H7(){
    spList = {FIRST_SPECIES};
    v_type = {0};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[problem->getSize()-2], IRT_EQ, 69); 
    return problem;
}

CounterpointProblem* FuxTest::test_1sp_H7_2(){
    spList = {FIRST_SPECIES};
    v_type = {-1};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[problem->getSize()-2], IRT_EQ, 55); 
    return problem;
}

CounterpointProblem* FuxTest::test_2sp_H2(){
    spList = {SECOND_SPECIES};
    v_type = {2};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    //these three lines are an example of the constraint allowing a dissonance
    //rel(problem->getHome(), problem->getSolutionArray()[2], IRT_EQ, 78); 
    //rel(problem->getHome(), problem->getSolutionArray()[3], IRT_EQ, 79); 
    //rel(problem->getHome(), problem->getSolutionArray()[4], IRT_EQ, 81); 
    //these three lines are an example of the constraint blocking a dissonance
    rel(problem->getHome(), problem->getSolutionArray()[2], IRT_EQ, 74); 
    rel(problem->getHome(), problem->getSolutionArray()[3], IRT_EQ, 79); 
    rel(problem->getHome(), problem->getSolutionArray()[4], IRT_EQ, 81); 
    return problem;
}

CounterpointProblem* FuxTest::test_3sp_H1(){
    spList = {THIRD_SPECIES};
    v_type = {2};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[4], IRT_EQ, 66);
    rel(problem->getHome(), problem->getSolutionArray()[5], IRT_EQ, 66);
    rel(problem->getHome(), problem->getSolutionArray()[6], IRT_EQ, 67);
    rel(problem->getHome(), problem->getSolutionArray()[7], IRT_EQ, 69);
    rel(problem->getHome(), problem->getSolutionArray()[8], IRT_EQ, 69);
    return problem;
}

CounterpointProblem* FuxTest::test_4sp_H2(){
    spList = {FOURTH_SPECIES};
    v_type = {-1};
    auto* problem = create_problem(cantusFirmus, spList, v_type, melodic_params, general_params, specific_params,
            importance, borrowMode);
    rel(problem->getHome(), problem->getSolutionArray()[1], IRT_EQ, 52);
    rel(problem->getHome(), problem->getSolutionArray()[2], IRT_EQ, 52);
    return problem;
}

vector<Species> FuxTest::getSpList(){
    return spList;
}

vector<int> FuxTest::getCf(){
    return cantusFirmus;
}

vector<int> FuxTest::getVType(){
    return v_type;
}

int FuxTest::getBMode(){
    return borrowMode;
}

vector<int> FuxTest::getCp(){
    return cp;
}

int FuxTest::getIdx(){
    return idx;
}

void FuxTest::test_2v_1sp_fig22_setter(int i){
    cout << "Fig. 22" << endl;
    spList = {FIRST_SPECIES};
    cantusFirmus = {57,60,59,62,60,64,65,64,62,60,59,57}; //1sp 2v cf
    cp =           {69,64,67,65,64,72,69,71,71,69,68,69};
    v_type = {1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_1sp_fig23_setter(int i){
    cout << "Fig. 23" << endl;
    spList = {FIRST_SPECIES};
    cantusFirmus = {57,60,59,62,60,64,65,64,62,60,59,57}; //1sp 2v cf
    cp =           {57,57,55,53,52,52,50,48,55,57,56,57};
    v_type = {-1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_2sp_fig38_setter(int i){
    cout << "Fig. 38" << endl;
    spList = {SECOND_SPECIES};
    cantusFirmus = {53,55,57,53,50,52,53,60,57,53,55,53}; //1sp 2v cf
    cp =           {0,65,
                    64,62,
                    60,58,
                    57,55,
                    53,57,
                    60,58,
                    57,69,
                    67,64,
                    65,67,
                    69,65,
                    62,64,
                    65};
    v_type = {1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_2sp_fig39_setter(int i){
    cout << "Fig. 39 (the voice types as defined previously don't work here)" << endl;
    spList = {SECOND_SPECIES};
    cantusFirmus = {53,55,57,53,50,52,53,60,57,53,55,53}; //1sp 2v cf
    cp =           {0,53,52,48,53,52,50,48,46,58,55,60,57,53,52,48,53,41,45,50,48,52,53};
    v_type = {-1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_2sp_fig40_setter(int i){
    cout << "Fig. 40" << endl;
    spList = {SECOND_SPECIES};
    cantusFirmus = {55,60,59,55,60,64,62,67,64,60,62,59,57,55}; //1sp 2v cf
    cp =           {0,67,64,65,67,69,71,69,67,72,71,72,74,72,71,69,67,65,64,72,71,69,67,62,64,66,67};
    v_type = {2};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_3sp_fig55_setter(int i){
    cout << "Fig. 55" << endl;
    spList = {THIRD_SPECIES};
    cantusFirmus = {62,65,64,62,67,65,69,67,65,64,62}; //1sp 2v cf
    cp =           {62,64,65,67,//ok
                    69,71,72,74,//ok
                    76,74,71,72,//ok
                    74,72,70,69,//ok
                    70,72,74,76,//ok
                    77,65,69,71,//ok, but check for maybe bemol on last note
                    72,69,70,72,//ok
                    70,69,67,70,//HERE FIRST NOTE ERROR
                    69,62,64,65,
                    67,69,71,73,
                    74};
    cout << "CP size : " << endl;
    cout << cp.size() << endl;
    v_type = {1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_4sp_fig74_setter(int i){
    cout << "Fig. 74" << endl;
    spList = {FOURTH_SPECIES};
    cantusFirmus = {62,   65,   64,   62,   67,   65,   69,   67,   65,   64,   62}; //1sp 2v cf
    cp =           {   50,50,62,62,60,60,59,59,64,64,62,62,65,65,64,64,62,62,61,62};
    cout << cp.size() << endl;
    v_type = {-1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_2v_5sp_fig82_setter(int i){
    cout << "Fig. 82" << endl;
    spList = {FIFTH_SPECIES};
    cantusFirmus = {62,   65,   64,   62,   67,   65,   69,   67,   65,   64,   62}; //1sp 2v cf
    cp =           {0,0,69,69,69,62,64,65,67,65,64,67,65,62, 74,74,74,72,70,67,69,71,72,72,72,72, 77,77,77,76,76,76,76,69, 74,74,74,74,73,73,74};
    cout << cp.size() << endl;
    v_type = {1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_1sp_fig108_setter(int i){
    cout << "Fig. 108" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {64,60,62,60,57,69,67,64,65,64};
    cp =           {67,64,65,69,72,72,76,72,69,68,
                    52,57,50,53,53,53,60,60,62,64};
    v_type = {1,-2};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_1sp_fig109_setter(int i){
    cout << "Fig. 109" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {52,48,50,48,45,57,55,52,53,52};
    cp =           {67,64,65,64,64,64,67,67,62,64,
                    59,60,57,57,60,60,59,59,57,59};
    v_type = {2,0};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_1sp_fig110_setter(int i){
    cout << "Fig. 110" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {65,67,69,65,62,64,65,72,69,65,67,65};
    cp =           {60,60,60,62,58,59,57,69,65,62,64,65,
                    53,52,53,50,55,55,53,45,50,50,48,41};
    v_type = {-1 ,-3};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_1sp_fig111_setter(int i){
    cout << "Fig. 111" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {65,67,69,65,62,64,65,72,69,65,67,65};
    cp =           {69,72,72,74,74,70,69,67,65,77,76,77,
                    65,64,65,62,58,55,53,52,53,50,48,53};
    v_type = {1 ,-2};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_2sp_fig125_setter(int i){
    cout << "Fig. 125" << endl;
    spList = {FIRST_SPECIES, SECOND_SPECIES};
    cantusFirmus = {52,   48,   50,   48,   45,   57,   55,   52,   53,   52};
    cp =           {64,   64,   65,   64,   65,   64,   67,   67,   69,   68,
                    00,59,60,59,57,53,55,52,57,59,60,57,59,62,64,62,60,57,59};
    v_type = {2 ,1};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_3v_4sp_fig146_setter(int i){
    cout << "Fig. 146" << endl;
    spList = {FOURTH_SPECIES, FIRST_SPECIES};
    cantusFirmus = {64,   60,   62,   60,   57,   69,   67,   64,   65,   64};
    cp =           {   76,76,72,72,71,71,69,69,72,72,74,74,72,72,69,69,71,68,
                    52,   57,   55,   57,   53,   53,   52,   48,   50,   52};
    v_type = {1 ,-2};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_4v_1sp_fig166_setter(int i){
    cout << "Fig. 166" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {64,60,62,60,57,69,67,64,65,64};
    cp =           {59,57,57,57,60,60,64,60,62,59,
                    56,57,53,52,52,53,60,60,57,56,
                    52,53,50,45,45,41,40,45,50,52};
    v_type = {-1 ,-1, -3};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_4v_1sp_fig167_setter(int i){
    cout << "Fig. 167" << endl;
    spList = {FIRST_SPECIES, FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {64,60,62,60,57,69,67,64,65,64};
    cp =           {68,69,65,64,64,65,72,72,69,68,
                    59,57,57,57,60,60,64,60,62,59,
                    52,53,50,45,45,53,52,57,50,52};
    v_type = {0 ,-1, -3};
    idx = i;
    borrowMode = 1;
}

void FuxTest::test_4v_2sp_fig176_setter(int i){
    cout << "Fig. 173" << endl;
    spList = {SECOND_SPECIES, FIRST_SPECIES, FIRST_SPECIES};
    cantusFirmus = {50,   53,   52,   50,   55,   53,   57,   55,   53,   52,   50};
    cp =           {00,62,60,62,64,67,65,64,62,64,65,69,64,65,67,62,65,69,67,64,66,
                    65,   69,   72,   74,   70,   69,   69,   71,   74,   73,   74,
                    57,   57,   55,   57,   58,   60,   60,   62,   57,   64,   57};
    v_type = {2, 3, 1};
    idx = i;
    borrowMode = 1;
}
// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the declarations of the functions that post the constraints.  
// 

#ifndef CONSTRAINTS_HPP
#define CONSTRAINTS_HPP

class Part;

#include "Parts/Part.hpp"

/* ================================================
 *              GENERAL CONSTRAINTS
 * ================================================
 */

/**
 * G4 : if a note is borrowed, then a cost must be set
 */
void G4_counterpointMustBeInTheSameKey(Home home, Part* part);

/**
 * G6 : chromatic melodies are prohibited
 */
void G6_noChromaticMelodies(Home home, Part* part, int mSpec);

/**
 * G7 : prefer smaller melodic intervals (and set tritones to be last resort)
 */
void G7_melodicIntervalsShouldBeSmall(Home home, Part* part, int mSpec);

/**
 * G9 : the last chord must be in the scale of the composition
 */
void G9_lastChordSameAsFundamental(Home home, Stratum* lowest, Part* cantusFirmus);

/* ================================================
 *              HARMONIC CONSTRAINTS
 * ================================================
 */

/**
 * 1.H1 : the harmonic intervals of the first note of each measure must be consonances
 */
void H1_1_harmonicIntervalsAreConsonances(Home home, Part* part);

void H1_3_fiveConsecutiveNotesByJointDegree(Home home, Part* part);

/**
 * 1.H2 : The first harmonic interval must be a perfect consonance in 2 voice compositions
 */
void H2_1_startWithPerfectConsonance(Home home, Part* part);

void H2_2_arsisHarmoniesCannotBeDisonnant(Home home, Part* part);

void H2_3_disonanceImpliesDiminution(Home home, Part* part);

/**
 * 1.H3 : The last chord must be a perfect consonance. This applies to 2 voices, the 3 and 4 voice version is applied in the stratum constructors
 */
void H3_1_endWithPerfectConsonance(Home home, Part* part);

void H3_2_penultimateNoteDomain(Home home, Part* part);

void H3_3_cambiataCost(Home home, Part* part);

//note : H5 was divided into 2 different ocnstraints since the cantusFirmus has a smaller notes array than the other parts

/**
 * 1.H5 : Voices cannot play the same notes (except for the first and last note) (cantusFirmus version)
 */
void H5_1_cpAndCfDifferentNotes(Home home, Part* part, Part* cf);

/**
 * 1.H5 : Voices cannot play the same notes (except for the first and last note) (inter-voices version)
 */
void H5_1_differentNotes(Home home, vector<Part*> part);

/**
 * 1.H6 : Imperfect consonances are preferred (sets costs for fifth and octave)
 */
void H6_1_preferImperfectConsonances(Home home, Part* part);

/**
 * 1.H7 : the penultimate note must be a major sixth or a minor third depending on if it is the lowest stratum or not (counterpoint version)
 */
void H7_1_2v_penultimateSixthOrThird(Home home, Part* part);

/**
 * 1.H8 : the harmonic triad should be used as much as possible (3 voices)
 */
void H8_3v_preferHarmonicTriad(Home home, Part* part, IntVarArray triadCostArray, Stratum* upper1, Stratum* upper2);

/**
 * 1.H8 : the harmonic triad should be used as much as possible (4 voices)
 */
void H8_4v_preferHarmonicTriad(Home home, IntVarArray triadCostArray, Stratum* upper1, Stratum* upper2, Stratum* upper3);

/* ================================================
 *              MELODIC CONSTRAINTS
 * ================================================
 */


/**
 * 1.M1 : melodic intervals cannot exceed a minor sixth
 */
void M1_1_2v_melodicIntervalsNotExceedMinorSixth(Home home, Part* part);

/**
 * 1.M1 : melodic intervals cannot exceed a minor sixth but may include an octave
 */
void M1_1_3v_melodicIntervalsNotExceedMinorSixth(Home home, Part* part);

/**
 * 2.M1 : If the two voices are getting so close that there is no contrary motion possible without crossing each other, then the melodic interval of 
 * the counterpoint can be an octave leap.
 */
void M1_2_octaveLeap(Home home, Part* part, Stratum* low);

void M2_2_2v_twoConsecutiveNotesAreNotTheSame(Home home, Part* part);

void M2_2_3v_melodicIntervalsNotExceedMinorSixth(Home home, vector<Part*> parts, bool containsThirdSpecies);

/**
 * 1.M2 : The notes of each part should be as diverse as possible
 */
void M2_1_varietyCost(Home home, vector<Part*> parts);

void P1_1_2v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void P1_1_4v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void P1_2_2v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void P1_2_3v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void P1_2_4v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void P3_0_noBattuta(Home home, Part* part);

void P3_1_noBattuta(Home home, Part* part);

void P3_2_noBattuta(Home home, Part* part);

void P4_successiveCost(Home home, vector<Part*> parts, int scc_cz, IntVarArray successiveCostArray, vector<Species> species);

void P6_noMoveInSameDirection(Home home, vector<Part*> parts);

void P7_noSuccessiveAscendingSixths(Home home, vector<Part*> parts);

void P1_1_3v_noDirectMotionFromPerfectConsonance(Home home, Part* part);

void H7_1_3v_penultimateSixthOrThird(Home home, Part* part);

/* ================================================
 *               OTHER CONSTRAINTS
 * ================================================
 */

/**
 * This function creates the isOff array to be able to know if a note is borrowed or not
 */
void initializeIsOffArray(Home home, Part* part);

/**
 * Two fifth species rhythms should be as diverse as possible
 */
void twoFifthSpeciesDiversity_3v(Home home, Part* cp1, Part* cp2);

/**
 * For the upper strata, there shouldn't be a minor second interval between the thesis notes
 */
void noMinorSecondBetweenUpper(Home home, vector<Part*> parts);

#endif
#ifndef RM_H
#define RM_H

#include <iostream>
#include <list>
/**
 * Integer Register R000 - R999
 * Float Register F000 - F999
 * R000 - R499: caller save
 * F000 - F499: caller save
 * R500 - R899: callee save
 * F500 - F899: callee save
 * R900 - R999: reserved for special use
 * Return Address: R900
 */

#define CALLER_START	0
#define CALLER_END	499
#define CALLEE_START	500
#define CALLEE_END	899

class RegManager {
 public:
  RegManager();
  int getReg(bool isCallerSave, bool isFloat);
  void releaseReg(int regNum, bool isFloat);
  bool isCallerSR(int regNum);
 private:
  /**
   * freeCallerSIR abbr. of free caller save Integer register.
   */
  std::list<int> freeCallerSIR_;
  std::list<int> freeCalleeSIR_;
  std::list<int> inUseCallerSIR_;
  std::list<int> inUseCalleeSIR_;

  std::list<int> freeCallerSFR_;
  std::list<int> freeCalleeSFR_;
  std::list<int> inUseCallerSFR_;
  std::list<int> inUseCalleeSFR_;
};

#endif

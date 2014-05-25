#include <iostream>
#include <map>
#include <vector>
#include <cstdlib>

using namespace std;

typedef unsigned SessionID;
typedef unsigned Index;

typedef struct {
  SessionID sid;
  Index idx;
} Addr;

typedef struct {
  bool isKnown;
  vector<Addr> vis;
} Effect;

typedef vector<vector<Effect> > State;

int main (int argc, char* argv[]) {
  if (argc != 5) {
    cerr << "USAGE: " << argv[0] << " num_sessions num_actions percent_vis percent_known" << endl;
    return 1;
  }

  int numSessions = atoi (argv[1]);
  int numActions = atoi (argv[2]);
  int percentVis = atoi (argv[3]);
  int percentKnown = atoi (argv[4]);

  State state;

  for (unsigned i = 0; i < numSessions; i++) {
    vector<Effect> v;
    state.push_back(v);
  }

  for (unsigned i = 0; i < numActions; i++) {
    unsigned session = rand () % numSessions;
    bool isKnown = (rand () % 100) < percentKnown;
    vector<Addr> vis;
    for (vector<vector<Effect> >::iterator sess = state.begin(); sess != state.end(); sess++) {
      for (vector<Effect>::iterator eff = (*sess).begin(); eff != (*sess).end(); eff++) {
        bool isVis = (rand () % 100) < percentVis;
        if (isVis) {
          Addr a;
          a.sid = sess - state.begin();
          a.idx = eff - (*sess).begin();
          vis.push_back (a);
        }
      }
    }
    Effect e = { isKnown, vis };
    state[session].push_back(e);
  }

  for (vector<vector<Effect> >::iterator sessIt = state.begin (); sessIt != state.end (); sessIt++) {
    for (vector<Effect>::iterator effIt = (*sessIt).begin(); effIt != (*sessIt).end(); effIt++) {
      cout << sessIt - state.begin() << ",\t" << effIt - (*sessIt).begin() << ",\t" << (*effIt).isKnown << ",\t[";
      vector<Addr> visSet = (*effIt).vis;
      for (vector<Addr>::iterator it = visSet.begin (); it != visSet.end(); it++) {
        cout << "(" << (*it).sid << "," << (*it).idx << ")";
        if (visSet.end () - it != 1)
          cout << ",";
      }
      cout << "]" << endl;;
    }
  }
  return 0;
}

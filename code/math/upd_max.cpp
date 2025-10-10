// --std=c++17
template<typename T,typename U>
void upd_max(T & a , U b) { if (a < b) a = b; } 

template<typename T,typename U,typename ... Args>
void upd_max(T & a , U b, Args... args) { if (a < b) a = b; upd_max(a,args...); } 


template<typename T,typename U>
void upd_min(T & a , U b) { if (a > b) a = b; }

template<typename T,typename U,typename ... Args>
void upd_min(T & a , U b, Args... args) { if (a > b) a = b; upd_min(a,args...); }

template<typename T,typename U,typename Compare = std::less<T>>
void upd(T & a , U b, Compare cmp = Compare()) { if (cmp(a,b)) a = b; }

template<typename T,typename U,typename... Args,typename Compare = std::less<T>>
void upd(T & a , U b, Args... args, Compare cmp = Compare()) { if (cmp(a,b)) a = b; upd(a,args...,cmp); }

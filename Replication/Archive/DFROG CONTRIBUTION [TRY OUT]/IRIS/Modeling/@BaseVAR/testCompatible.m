function flag = testCompatible(v1, V2)
% testCompatible  True if two BaseVAR objects can occur together on the LHS and RHS in an assignment
%
% Backend IRIS function.
% No help provided.

% -IRIS Macroeconomic Modeling Toolbox.
% -Copyright (c) 2007-2020 IRIS Solutions Team.

%--------------------------------------------------------------------------

try
    flag = isequal(class(v1), class(V2)) ...
        && isequal(v1.AllNames, V2.AllNames) ...
        && isequal(v1.Zi, V2.Zi) ...
        && ranger.eq(v1.Range, V2.Range) ...
        && size(v1.A, 1) == size(V2.A, 1) ...
        && size(v1.A, 2) == size(V2.A, 2) ...
        && size(v1.K, 1) == size(V2.K, 1) ...
        && size(v1.K, 2) == size(V2.K, 2) ...
        && size(v1.J, 1) == size(V2.J, 1) ...
        && size(v1.J, 2) == size(V2.J, 2);        
catch %#ok<CTCH>
    flag = false;
end

end

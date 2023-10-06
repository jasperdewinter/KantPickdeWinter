function M = checknan(A,BP)

% This function is another way of offseting a but in the Mathworks code

M=A; m=size(M,1); 
if sum(sum(isnan(M),1),2)~=0
    for i=1:m
        if sum(isnan(M(i,:)),2)~=0
            for j=1:m
                if isnan(M(i,j))==1
                    u=BP(i,:).*BP(j,:);
                    u=sort(u);
                    M(i,j)=sum(u);
                end
            end
        end
    end
end


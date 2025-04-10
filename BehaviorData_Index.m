clc; clear;
basepath = "C:\Users\XinHao\Desktop\2025_SCZ_AI\BehaviorData";
subfolders = dir(basepath);
subfolders = subfolders([subfolders.isdir]);  
subfolders = subfolders(~ismember({subfolders.name}, {'.', '..'})); 

for i = 1:numel(subfolders)
    subfolderName = subfolders(i).name;
    subfolderPath = fullfile(basepath, subfolderName);
    cd(subfolderPath);
    tiqu = @(x)(split(x,":"));
    shuju = @(x)(str2double(x{end,1}));
    trials = dir(subfolderPath+'/2_*_*_*.mat');
    pair=table;
    idx = 0;
    
    for n = 1:2:numel(trials)
        idx = (n-1)/2+1;
        k = strfind(trials(n).name,'_');
        a = trials(n).name(1:k(2));
        b = trials(n+1).name(1:k(2));
        
        k = strfind(trials(n).name,'_'); 
        a = trials(n).name(1:k(2)); 
        load(trials(n).name); load(trials(n+1).name);

        pair.cond(idx) = str2double(a(1));
        pair.trial(idx) = str2double(a(3:end-1));
        
        y{end} = [];
        y = y(cellfun(@isempty,y)==0);
        y = cellfun(@string,y);
        x{end} = [];
        x = x(cellfun(@isempty,x)==0);
        x = cellfun(@string,x);
        y = cellfun(tiqu,y,'UniformOutput',false);
        x = cellfun(tiqu,x,'UniformOutput',false);
        
        B.RT = cellfun(shuju,y);
        A.RT = cellfun(shuju,x);
        A.RT(isnan(A.RT))=[];B.RT(isnan(A.RT))=[];
        B.IOI = diff(B.RT);
        A.IOI = diff(A.RT);
        B.Trials(:,idx) = {B.IOI};
        A.Trials(:,idx) = {A.IOI};
        B.RT=cumsum(B.IOI);
        A.RT=cumsum(A.IOI);
        
        if A.RT(1)<250 || B.RT(1)<250
            A.IOI(A.RT<250)=[]; B.IOI(B.RT<250)=[];
            B.RT=cumsum(B.IOI);A.RT=cumsum(A.IOI);
        end
                
        if A.RT(1)>2000 || B.RT(1)>2000
            pair.deletion8(idx)=true;
        else
            pair.deletion8(idx)=false;
        end
        
        if numel(B.RT)<numel(A.RT) 
            A.IOI(numel(B.RT)+1:end)=[];
            A.RT(numel(B.RT)+1:end)=[];
        else
            B.IOI(numel(A.RT)+1:end)=[];
            B.RT(numel(A.RT)+1:end)=[];
        end
        
         points = numel(A.RT);
         pair.Aoutlier(idx,1)  = sum(nonzeros(A.IOI<=median(A.IOI)-0.5*median(A.IOI)));
         pair.Aoutlier(idx,2) = sum(nonzeros(A.IOI>=median(A.IOI)+0.5*median(A.IOI)));
         pair.Boutlier(idx,1) = sum(nonzeros(B.IOI<=median(B.IOI)-0.5*median(B.IOI)));
         pair.Boutlier(idx,2) = sum(nonzeros(B.IOI>=median(B.IOI)+0.5*median(B.IOI)));
        
         % index1: stability original value
         pair.WS(idx,:) = sqrt(1 / std(B.IOI));
         % index2: consistency original value
         phaseA = t2phases(cumsum(500 * ones(points, 1)), A.RT);
         phaseB = t2phases(cumsum(500 * ones(points, 1)), B.RT);
         phase2keep = min(length(phaseA), length(phaseB));
         dtheta = phaseA(1:phase2keep) - phaseB(1:phase2keep);
         pair.IC(idx,:) = abs(mean(exp(1i * dtheta)));
    end
    pair_cell = [pair.Properties.VariableNames; table2cell(pair)];
    xlswrite(fullfile(subfolderPath, [subfolderName, '.xlsx']), pair_cell);

end

%% func_t2phases
function theta2 = t2phases(metronome, T)
    count=1;
    jstart=find(metronome-T(1)>=0, 1);
    j=jstart;
    n=1;
    theta(1:jstart-1)=NaN;
    while ~isempty(n) && n<(length(T)-1)
            if j > size(metronome, 1)
                break; 
            end
            n=find(sign(metronome(j)-T)==-1, 1)-1;
            if  (isempty(n)==0)
                count=count+1;
                theta(j)=(metronome(j)-T(n))/(T(n+1)-T(n))*2*pi+2*pi*n;
                j = j + 1;
            end
    end
    temp=isnan(theta);
    theta2=theta(temp==0);
end
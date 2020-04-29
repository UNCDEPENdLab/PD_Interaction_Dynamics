function moveSelectedLine ()

global EKG;

if EKG.rspBoundExists == 1

if (get(findobj('Tag', 'lowerBound'), 'color')) == [0 1 1]
    
delete(findobj('Tag', 'lowerBound')) % remove line if present
axes(findobj('Tag', 'AxesRspPlot'));
pointDown = get(gca,'CurrentPoint');
pointDown = pointDown(1,1);
EKG.RSPpointDown = pointDown;

line([pointDown pointDown],[EKG.rspMin EKG.rspMax],'color',[1 1 1], 'userdata',[0 1 1], 'LineWidth', 1.25, ...
                     'parent',gca, 'Tag', 'lowerBound',...
                     'buttondownfcn', ...
					['tmpstrr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstrr); clear tmpstrr;' ...
                     ] ); 
end

if (get(findobj('Tag', 'upperBound'), 'color')) == [0 1 1]
    
delete(findobj('Tag', 'upperBound')) % remove line if present
axes(findobj('Tag', 'AxesRspPlot'));
pointUp = get(gca,'CurrentPoint');
pointUp = pointUp(1,1); 
EKG.RSPpointUp = pointUp;
                 
line([pointUp pointUp],[EKG.rspMin EKG.rspMax],'color',[1 1 1], 'userdata',[0 1 1], 'LineWidth', 1.25, ...
                     'parent',gca, 'Tag', 'upperBound',...
                     'buttondownfcn', ...
					['tmpstrr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstrr); clear tmpstrr;'] );  
       
end
 drawPsdPlot;  
end





#lang scribble/manual

@title{Clicker}

The Clicker module of Conspiracy provides label, button and container widgets for 2htdp/image.

@verbatim|{
> (require conspiracy/clicker)
}|

@section{Clicker Model}

The library provides simple support for positioning buttons within a 2htdp scene and handling the "button up" mouse event that occurs within the boundaries of active buttons within active containers. A container can be thought of as a group of associated buttons.

@subsection{label}

@bold{Syntax:}
@verbatim|{
(|@bold{label} |@italic{name}
       |@italic{border?}
       |@italic{font-size}
       |@italic{font-color}
       |@italic{bg-color}
       |@italic{padding})
  |@italic{name}       : (or/c string? image?)
  |@italic{border?}    : boolean?
  |@italic{font-size}  : (or/c #f  (and/c integer? (between/c 1 255)))
  |@italic{font-color} : image-color?
  |@italic{bg-color}   : (or/c pen? image-color?)
  |@italic{padding}    : (or/c #f nonnegative-integer?)
}|

Defines a text or image label for a button or container. The label may have a border , and if the 

@subsection{button}

@bold{Syntax:}
@verbatim|{
(|@bold{button} |@italic{name}
         |@italic{active?}
         |@italic{label}
         |@italic{up-action})
  |@italic{name}     : (or/c integer? symbol? string?)
  |@italic{active?}    : boolean?
  |@italic{label}      : (or/c #f label?)
  |@italic{up-action}  : procedure?
}|

Defines a button. If active the up-action procedure will be evaluated when the "up action" mouse event is handled within the border of the button. Label is displayed within the border of the button. The button's position and dimensions are controlled by its parent container. 

@subsection{container}

@bold{Syntax:}
@verbatim|{
(|@bold{container} |@italic{name}
           |@italic{active?}
           |@italic{x-offset}
          |@italic{y-offset}
          |@italic{bg-color}
           |@italic{border?}       
           |@italic{label}
           |@italic{label-height}
           |@italic{button-vertical?}
           |@italic{button-border?}
           |@italic{buttons-x-padding}
           |@italic{buttons-y-padding}
           |@italic{button-width}
           |@italic{button-height}
           |@italic{button-label-defaults}
           |@italic{activate}
           |@italic{deactivate}
           |@italic{buttons})
  |@italic{name}                 : (or/c integer? symbol? string?)
  |@italic{active?}               : boolean?
  |@italic{x-offset}              : integer?
  |@italic{y-offset}              : integer?
  |@italic{bg-color}              : (or/c pen? image-color?)
  |@italic{border?}               : boolean?
  |@italic{label}                 : (or/c #f label?)
  |@italic{label-height}          : (or/c #f positive-integer?)
  |@italic{buttons-vertical?}      : boolean?
  |@italic{buttons-border?}       : boolean?
  |@italic{buttons-x-padding}     : nonnegative-integer?
  |@italic{buttons-y-padding}     : nonnegative-integer?
  |@italic{button-width}          : positive-integer?
  |@italic{button-height}         : positive-integer?
  |@italic{button-label-defaults} : (or/c #f label?)
  |@italic{activate}              : (or/c #f procedure?)
  |@italic{deactivate}            : (or/c #f procedure?)
  |@italic{bottons}               : (non-empty-listof button?)
}|

Creates a container of associated buttons. If the container is active then mouse events occurring within th container will be handled by the appropriate active button within the x and y border defined by the button. A container can have a border, a label, background color and organize its associated buttons in either horizontal or vertical positions.

@subsection{place-containers}

@bold{Syntax:}
@verbatim|{
(|@bold{place-containers} |@italic{containers}
         |@italic{img})
  |@italic{containers}     : (listof container?)
  |@italic{img}           : image?  
}|

Places the active containers, and their associated active buttons, onto the specified locations of @italic{img} and returns a new image made from the composite.

@subsection{process-containers}

@bold{Syntax:}
@verbatim|{
(|@bold{process-containers} |@italic{containers}
         |@italic{ws}
         |@italic{x}
         |@italic{y}
         |@italic{evt})
  |@italic{containers}     : (listof container?)
  |@italic{ws}           : any/c
  |@italic{x}            : integer?
  |@italic{y}            : integer?
  |@italic{evt}          : (or/c "button-down" "button-up" "drag" "move" "enter" "leave")
}|

Processes mouse "button up" events for all active buttons within active containers.

@subsection{select-container/button}

@bold{Syntax:}
@verbatim|{
(|@bold{select-container/button} |@italic{containers}
         |@italic{ws}
         |@italic{x}
         |@italic{y}
         |@italic{evt})
  |@italic{containers}     : (listof container?)
  |@italic{ws}           : any/c
  |@italic{x}            : integer?
  |@italic{y}            : integer?
  |@italic{evt}          : (or/c "button-down" "button-up" "drag" "move" "enter" "leave")
}|

Selects the active container and button for the given mouse event. This is the active button within the active contaier whose boundaries enclose the x and y positions of the mouse event. 

Returns #f if no active container/button found. Otherwise returns a list of the container and button.

@subsection{find-container}

@bold{Syntax:}
@verbatim|{
(|@bold{find-container} |@italic{ctn-name}
         |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{containers}     : (listof container?)  
}|

Returns the container associated with ctn-name in the list of containers. If no container is found matching ctn-name then #f is returned.

@subsection{activate-container}

@bold{Syntax:}
@verbatim|{
(|@bold{activate-container} |@italic{ctn-name}
                  |@italic{ws}
                  |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{ws}            : any/c
  |@italic{containers}     : (listof container?)  
}|

Activates the matching container. If the container defines an activate function then this is passed the world-state and its result returned.

@subsection{deactivate-container}

@bold{Syntax:}
@verbatim|{
(|@bold{deactivate-container} |@italic{ctn-name}
                      |@italic{ws}
                      |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{ws}            : any/c
  |@italic{containers}     : (listof container?)  
}|

Deactivates the matching container. If the container defines a deactivate function then this is passed the world-state and its result returned.

@subsection{find-container/button}

@bold{Syntax:}
@verbatim|{
(|@bold{find-container/button} |@italic{ctn-name}
                      |@italic{btn-name}
                      |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{btn-name}      : (or/c symbol? integer?)
  |@italic{containers}     : (listof container?)  
}|

Returns a list of the container and button associated with ctn-name and btn-name in the list of containers. If no container is found matching ctn-name and btn-name then #f is returned.

@subsection{find-button}

@bold{Syntax:}
@verbatim|{
(|@bold{find-button} |@italic{ctn-name}
             |@italic{btn-name}
             |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{btn-name}      : (or/c symbol? integer?)
  |@italic{containers}     : (listof container?)  
}|

Returns the button associated with ctn-name and btn-name in the list of containers. If no container is found matching ctn-name and btn-name then #f is returned.

@subsection{activate-button}

@bold{Syntax:}
@verbatim|{
(|@bold{activate-button} |@italic{ctn-name}
                |@italic{btn-name}
                |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{btn-name}      : (or/c symbol? integer?)
  |@italic{containers}     : (listof container?)  
}|

Activates the matching button for the matching container.

@subsection{deactivate-button}

@bold{Syntax:}
@verbatim|{
(|@bold{deactivate-button} |@italic{ctn-name}
                  |@italic{btn-name}
                  |@italic{containers})
  |@italic{ctn-name}      : (or/c symbol? integer?)
  |@italic{btn-name}      : (or/c symbol? integer?)
  |@italic{containers}     : (listof container?)  
}|

Deactivates the matching button for the matching container. 
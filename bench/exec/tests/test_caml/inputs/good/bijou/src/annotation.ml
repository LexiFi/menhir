type ('tag, 'a) t = {
  tag: 'tag;
  content: 'a
}

let make tag content = {
  tag = tag;
  content = content
}

let transfer { tag = tag } content = {
  tag = tag;
  content = content
}

let content { content = content } =
  content

let get { tag = tag } =
  tag

let map f tv =
  transfer tv (f (content tv))

let iter f tv =
  f (content tv)

let fold f accu tv =
  f accu (content tv)

let fold2 f accu tv1 tv2 =
  f accu (content tv1) (content tv2)


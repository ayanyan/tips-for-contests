class MyHeap

  def initialize uniq=false, &cmp
    @tree = Array.new
    @tree.push nil
    @cmp = cmp || lambda { |x, y| x <=> y }
    @uniq = uniq
  end

  def push x
    @tree.push x
    shiftup (@tree.length - 1)
    self
  end

  def pop rec=true
    return nil unless 1 < @tree.length
    v = @tree[1]
    @tree[1] = @tree[-1]
    @tree.pop
    shiftdown 1
    if @uniq and rec
      while top == v
        v = pop false
      end
    end
    v
  end

  def flat_push y
    y.each do |x|
      push x
    end
    self
  end

  def each_pop &fun
    while x = pop
      fun.call x
    end
  end

  def top
    @tree[1]
  end

  def size
    @tree.length - 1
  end

  private

  def swap n, m
    tmp = @tree[n]
    @tree[n] = @tree[m]
    @tree[m] = tmp
  end

  def shiftup n
    return unless 1 < n
    m = n / 2
    if @cmp.call(@tree[n], @tree[m]) < 0
      swap n, m
      shiftup m
    end
  end

  def shiftdown n
    m = 2 * n
    return unless m < @tree.length
    if @tree[m+1] and @cmp.call(@tree[m+1], @tree[m]) < 0
      m = m + 1
    end
    if @cmp.call(@tree[m], @tree[n]) < 0
      swap m, n
      shiftdown m
    end
  end

end

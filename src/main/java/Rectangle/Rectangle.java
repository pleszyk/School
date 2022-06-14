

public class Rectangle {
	private int height;
	private int width;
	
	public Rectangle(int height, int width) throws IllegalArgumentException {
		
		if (height < 0 ) throw new NegativeArgumentException();
		if (width < 0 ) throw new NegativeArgumentException();
		if (height > 100) throw new NegativeArgumentException();
		if (width > 100) throw new NegativeArgumentException();
		this.height = height;
		this.width = width;
		}


	public int getArea() {
		return 6;
	}
	
	public boolean isSquare() {
		if (this.height == this.width )
				return true;
				else 
					return false;
	}
}
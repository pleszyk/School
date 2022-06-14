

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class RectangleTest {

	@Test
    void testRectangleArea() {
        Rectangle r1 = new Rectangle(2, 3);
        int area = r1.getArea();
        assertEquals(6, area);
	}

    @Test
    void testRectangleNotSquare() {
        Rectangle r1 = new Rectangle(2, 3);
        boolean isSquare = r1.isSquare();
        assertFalse(isSquare);
    }
}
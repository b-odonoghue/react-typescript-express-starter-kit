import { ComplexNumber, ComplexAdditiveIdentity, ComplexMultiplicativeIdentity } from '../public/math/ComplexNumber';
import {expect} from 'chai';
import './support';

describe('ComplexNumber', () => {

    describe('constructor', () => {
        it('takes two numbers, the real and imaginary parts', () => {
            expect(new ComplexNumber(3,4)).to.be.an.instanceOf(ComplexNumber);
        });
    });

    describe('toString()', () => {
        context(`when realPart is 5 and imaginaryPart is -2`, () => {
            it(`should yield 5 - 2i`, () => {
                expect(new ComplexNumber(5,-2).toString()).to.equal('5 - 2i');
            });
        });

        context(`when realPart is 5 and imaginaryPart is -1`, () => {
            it(`should yield 5 - i`, () => {
                expect(new ComplexNumber(5,-1).toString()).to.equal('5 - i');
            });
        });

        context(`when realPart is 0 and imaginaryPart is 1`, () => {
            it(`should yield i`, () => {
                expect(new ComplexNumber(0,1).toString()).to.equal('i');
            });
        });

        context(`when realPart is 0 and imaginaryPart is -1`, () => {
            it(`should yield i`, () => {
                expect(new ComplexNumber(0,-1).toString()).to.equal('-i');
            });
        });

        context(`when realPart is 5 and imaginaryPart is 2`, () => {
            it(`should yield 5 + 2i`, () => {
                expect(new ComplexNumber(5,2).toString()).to.equal('5 + 2i');
            });
        });

        context(`when realPart is 0 and imaginaryPart is 2`, () => {
            it(`should yield 2i`, () => {
                expect(new ComplexNumber(0,2).toString()).to.equal('2i');
            });
        });

        context(`when realPart is 0 and imaginaryPart is 0`, () => {
            it(`should yield 0`, () => {
                expect(new ComplexNumber(0,0).toString()).to.equal('0');
            });
        });

        context(`when realPart is 2 and imaginaryPart is 0`, () => {
            it(`should yield 2`, () => {
                expect(new ComplexNumber(2,0).toString()).to.equal('2');
            });
        });

        context(`when realPart is 0 and imaginaryPart is -7`, () => {
            it(`should yield -7i`, () => {
                expect(new ComplexNumber(0,-7).toString()).to.equal('-7i');
            });
        });
    });

    const c1 = new ComplexNumber(5,-2);
    const c2 = new ComplexNumber(-7,5);

    describe('plus', () => {
        context('when 5-2i add -7+5i', () => {
            it(`should yield -2 + 3i`, () => {
                expect(c1.plus(c2)).to.oequal(new ComplexNumber(-2,3));
            });
        });
    });

    describe('additiveInverse', () => {
        context('when 5-2i add -5+2i', () => {
            it(`should yield 0`, () => {
                expect(c1.plus(c1.additiveInverse)).to.oequal(ComplexAdditiveIdentity);
            });
        });
    });

    describe('multiplicativeInverse', () => {
        context('given a number and its multiplicativeInverse', () => {
            it(`should yield 1`, () => {
                expect(c1.times(c1.multiplicativeInverse)).to.oequal(ComplexMultiplicativeIdentity);
            });
        });
    });
    
    describe('times', () => {
        context('when 5-2i times 5+2i', () => {
            it(`should yield 29`, () => {
                expect(c1.times(new ComplexNumber(5,2)).equals(29)).to.be.true;
            });
        });
    });
    
    describe('minus', () => {
        context('when 5-2i minus 2+4i', () => {
            it(`should yield 3 - 6i`, () => {
                expect(c1.minus(new ComplexNumber(2,4))).to.oequal(new ComplexNumber(3, -6));
            });
        });
    });
    
    describe('dividedBy', () => {
        context('when 5-2i minus 2+4i', () => {
            it(`should yield 3 - 6i`, () => {
                expect(c1.dividedBy(new ComplexNumber(2,2))).to.oequal(new ComplexNumber(3/4, -7/4));
            });
        });
    });

});
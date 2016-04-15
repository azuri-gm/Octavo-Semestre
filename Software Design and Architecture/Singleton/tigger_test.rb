# File: tigger_test.rb

require 'minitest/autorun'
require 'tigger'

class TiggerTest < Minitest::Test


  def test_tigger
    t = Tigger.instance
    assert_same(t, Tigger.instance)
    assert_raises(NoMethodError) do # "new" method should be private!
      Tigger.new
    end
    assert_equal("I'm the only one!", t.to_s)
    assert_equal('Grrr!', t.roar)
  end

end
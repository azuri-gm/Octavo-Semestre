Shoes.app :tittle => "Bla bla" do
	class Actions
	@myApp

	def initialize(myApp)
		@myApp = myApp
	end

	def doLogin(username, password)
		@myApp.app do 
			if username == "Eduardo" and password == "chen"
				alert "Successful Login"
			else 	
				alert "Incorrect Login"
			end
		end
	end

	stack do
		@myActions = Actions.new(self)
		username = edit_line
		password = edit_line

		button "Login" do
			@myActions.doLogin(username.text, password.text)
		end
	end
end
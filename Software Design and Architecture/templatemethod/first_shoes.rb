Shoes.app :tittle => "First Ruby GUI in Shoes" do
	class 
		@myApp
		def initialize(myApp)
			@myApp = myApp
		end

		def doLogin(username,password)
			@myApp.app do
				if username == "Chan" and password == "chen"
					alert "Successful Login"
				else
					alert "Incorrect username or password"
				end
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
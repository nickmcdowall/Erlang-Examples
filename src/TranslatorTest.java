import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public class TranslatorTest {

	private OtpSelf client;
	private OtpPeer server;
	private OtpConnection connection;

	@Before
	public void init() throws IOException, OtpAuthException {
		client = new OtpSelf("client", "batman");
		server = new OtpPeer("server@Nick-HP");
		connection = client.connect(server);
	}

	@Test
	public void shouldInvokeErlangTranslateFunction() throws IOException,
			OtpAuthException, OtpErlangExit, OtpErlangDecodeException {
		
		connection.sendRPC("translator", "translate", withArgs("friend", "Spanish"));
		OtpErlangObject response = connection.receiveMsg().getMsg();
		assertTrue(response.toString().contains("amigo"));
	}

	private OtpErlangObject[] withArgs(String word, String language) {
		return new OtpErlangObject[] { 
				new OtpErlangAtom(word),
				new OtpErlangAtom(language) 
		};
	}

}

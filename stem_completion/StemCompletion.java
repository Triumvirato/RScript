package stem_completion;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class StemCompletion
{
	private static final int DEFAULT_NUM_THREADS = 1;
	protected String[] documents;
	private String[] dictionary;
	protected String[] result;
	protected int numOfThreads;
	
	public StemCompletion(String[] documents, String[] dictionary)
	{
		init(documents, dictionary, DEFAULT_NUM_THREADS);
	}
	
	public StemCompletion(String[] documents, String[] dictionary, int numThreads)
	{
		init(documents, dictionary, numThreads);
	} 
	
	private void init(String[] documents,String[] dictionary,int numThreads)
	{
		this.documents = documents;
		this.dictionary = dictionary;
		this.numOfThreads = numThreads;
		this.result = new String[documents.length];
	}
	
	public String[] stemCompletion() throws Exception
	{
		int numDocs = documents.length;
		/*
		File f = new File("C:/Users/Andrea/Desktop/log.txt");
		BufferedWriter b = new BufferedWriter(new FileWriter(f));
		b.write("num docs " + new Integer(numDocs).toString());
		for(int j=0;j<numDocs;j++)
			b.append("\ndocument " + j + ": " + documents[j]);
		b.close();
		*/
		if(numDocs != 0)
		{
			for (int i = 0; i < numDocs; i++)
			{
				String[] words = documents[i].trim().split(" ");
				String docResult = " ";

				for (String word : words)
				{
					boolean completionFound = false;
					for (String dictWord : dictionary)
					{
						if (dictWord.startsWith(word))
						{
							docResult += " " + dictWord;
							completionFound = true;

							break;
						}
					}

					if(!completionFound)
					{
						docResult += " " + word;
					}
				}

				StemCompletion.this.result[i] = docResult.trim();
			}
		
	
			
		}
		
		return this.result;
	}

}
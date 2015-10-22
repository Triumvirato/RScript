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
		// numDocs indica il numero dei documenti
		int numDocs = documents.length;
		
		// se ci sono documenti
		if(numDocs != 0)
		{
			// ciclo sui documenti
			for (int i = 0; i < numDocs; i++)
			{
				// creo un array di stringhe in cui ogni elemento
				// e' una singola parola del documento corrente
				String[] words = documents[i].split(" ");
				
				// docResult sara' l'array che conterra' le stringhe completate
				// del documento corrente
				String docResult = " ";

				// ciclo sulle parole di un singolo documento
				for (String word : words)
				{
					boolean completionFound = false;
					
					// cerco se nel dizionario è presente una parola
					// che inizia con la parola corrente ( word )
					for (String dictWord : dictionary)
					{
						// se la trovo, sostituisco la parola corrente
						// con quella presente nel dizionario
						if (dictWord.startsWith(word))
						{
							docResult +=" " +dictWord;
							completionFound = true;

							break;
						}
					}
					
					// se non la trovo, lascio la parola cosi' com'è
					if(!completionFound)
					{
						docResult += " " + word;
					}
				}

				// sostituisco il documento corrente originale
				// con quello completato
				StemCompletion.this.result[i] = docResult;
			}
			
		}
		return this.result;
	}

}
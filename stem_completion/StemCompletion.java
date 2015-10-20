package stem_completion;
import java.util.Arrays;

public class StemCompletion
{
	private static final int DEFAULT_NUM_THREADS = 1;
	protected String[] documents;
	private String[] dictionary;
	volatile String[] result;
	protected int numOfThreads;
	
	public StemCompletion(String[] documents, String[] dictionary)
	{
		init(documents, dictionary, DEFAULT_NUM_THREADS);
	}
	
	public StemCompletion(String[] documents, String[] dictionary, int numThreads)
	{
		init(documents, dictionary, numThreads);
	} 
	
	private void init(String[] documents,String[] dictionary,int  numThreads)
	{
		this.documents = documents;
		this.dictionary = dictionary;
		this.numOfThreads = numThreads;
		this.result = new String[documents.length];
	}
	
	public String[] stemCompletion()
	{
		int numDocs = documents.length;
		
		if(numDocs != 0)
		{
			if(numDocs < numOfThreads)
			{
				numOfThreads = numDocs;
			}
			int quotient = numDocs / numOfThreads;
			int begin = 0;
			int end = quotient = - 1;
			StemCompletionThread[] threads = new StemCompletionThread[numOfThreads];
			
			int i;
			
			for(i = 0; i < threads.length - 1; i++)
			{
				StemCompletionThread dt = new StemCompletionThread(begin,end);
				dt.start();
				threads[i] = dt;
				begin = end + 1;
				end = end + quotient;
			}

				StemCompletionThread dt = new StemCompletionThread(begin, numDocs - 1);
				dt.start();
				threads[i] = dt;

				for (StemCompletionThread t : threads){
					try{
						t.join();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
		}
				return this.result;
	}

				class StemCompletionThread extends Thread {

					private int begin;
					private int end;

					public StemCompletionThread(int begin, int end) {
						//TODO Auto-generated constructor stud
						this.begin = begin;
						this.end = end;
					}

					@Override
					public void run() {
						//TODO Auto-generated method stub
						for (int i = begin; i <= end; i++){
							String[] words = documents[i].trim().split(" +");
							String docResult = " ";

							for (String word : words){
								boolean completionFound = false;
								for (String dictWord : dictionary){
									if (dictWord.startsWith(word)){
										docResult += " " + dictWord;
										completionFound = true;

										break;
									}
								}

								if(!completionFound){
									docResult += " " + word;
								}
							}

							StemCompletion.this.result[i] = docResult.trim();
						}
					}
				}
}
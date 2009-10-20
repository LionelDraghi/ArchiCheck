package rene.util;

import java.io.*;
import java.util.*;

import rene.util.sort.*;

class SortFile extends File
	implements SortObject
{	String S;
	public SortFile (File dir, String name)
	{	super(dir,name);
		try
		{	S=getCanonicalPath().toUpperCase();
		}
		catch (Exception e)
		{ S=""; }
	}
	public int compare (SortObject o)
	{	SortFile f=(SortFile)o;
		return -f.S.compareTo(S);
	}
}

class FileFilter
{	char F[][];
	public FileFilter (String s)
	{	StringTokenizer t=new StringTokenizer(s);
		int n=t.countTokens();
		F=new char[n][];
		for (int i=0; i<n; i++)
		{	F[i]=t.nextToken().toCharArray();
		}
	}
	public char[] filter (int i)
	{	return F[i];
	}
	public int filterCount ()
	{	return F.length;
	}
}

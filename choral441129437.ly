
\version "2.22.2"
\score {
    <<
         \new Staff << \clef "treble"
            \new Voice = "treble" { \set Staff.midiInstrument = #"choir aahs" \voiceOne 	a'4 d'' c'' a' c'' a' d'' c'' bes' a'2 r4

		g'4 bes' bes' c'' c'' c'' c'' a' a' a' g' c'' a' g' g'2 r4

		a'4 d'' d'' bes' bes' a' g' f' a' c'' c''2 r4

		a'4 bes' a' f' g' g' f' a' c'' d'' f'' e'' f'' c''2 r4

		a'4 c'' bes' c'' c'' bes' a' g' fis' bes' a' g' c'' bes'2 r4

		bes'4 d'' a' fis' d' g' bes' bes' a' bes' bes' c''2 r4

		g'4 g' a' bes' bes' cis'' a' c'' g' b' d'' c'' c'' b' e''2 r4

		g'4 a' c'' g' b' a' g' c'' c'' c'' a'2 r4
 }
            \new Voice = "contra" { \set Staff.midiInstrument = #"choir aahs" \voiceTwo 	f'4 f' e' c' c' c' d' e' e' f'2 r4

		e'4 f' d' f' e' f' c' c' f' f' e' f' f' f' e'2 r4

		f'4 f' d' d' c' c' c' c' f' g' e'2 r4

		d'4 bes cis' d' e' e' d' f' a' g' a' g' f' a'2 r4

		f'4 a' f' e' f' d' c' c' c' d' d' ees' fis' g'2 r4

		d'4 d' c' c' bes bes bes ees' f' f' e' f'2 r4

		e'4 e' f' f' e' e' d' c' e' g' g' g' f' g' g'2 r4

		e'4 c' c' cis' d' d' d' f' e' f' c'2 r4
 }
         >>
         \new Staff << \clef "bass"
            \new Voice = "tenor" { \set Staff.midiInstrument = #"choir aahs" \voiceOne 	a4 bes g a a a bes g e a2 r4

		g4 f g a g f e a c' c' bes c' c' b c'2 r4

		a4 a g f e f e a a g g2 r4

		a4 g a a bes cis' a f c' g c' c' c' a2 r4

		a4 a a g f f f e fis g fis g a bes2 r4

		bes4 bes a a g g f g a d' bes a2 r4

		g4 e a d' bes a f e e b b e' c' d' e'2 r4

		g4 f g g g f g a g f f2 r4
 }
            \new Voice = "bass" { \set Staff.midiInstrument = #"choir aahs" \voiceTwo 	f,4 d e f ees c bes, c c f2 r4

		e,4 d bes, a, e, a, c f a f g a a, f c2 r4

		f,4 d bes, g, e, a, c f f, e c2 r4

		fis,4 g, g, d g a d d c bes, a, c f f2 r4

		f,4 f, bes, bes, f, bes, f, c a, g, c ees d g2 r4

		g,4 bes, f, fis, g, bes, d c f bes g, f2 r4

		e,4 c f bes d cis d bes, c d g c' a b c'2 r4

		e,4 f, e, cis b, d bes, a, c a, f,2 r4
 }
         \new Lyrics \lyricsto "bass" { "I" "IV6" "V6" "I" "viiø43/IV" "viio6/IV" "IV" "V" "V7" "I" "V6" "IV6" "ii65" "I6" "V65" "I6" "V7" "I" "I6" "I" "viio6" "I6" "I6" "V42/V" "V" "I" "vi" "ii65" "ii7" "V65" "I6" "V7" "I" "I" "V6" "V" "V65/ii" "ii" "V42/vi" "vi" "iiø65/vi" "V7/vi" "vi" "vi" "iii6" "ii6" "I6" "V7" "I" "I" "I" "I" "IV7" "V42" "I" "IV" "I" "V" "viio6/ii" "ii" "V42/ii" "iv6/ii" "V7/ii" "ii" "ii" "bIII/ii" "bVII/ii" "V65/ii" "ii" "ii6" "IV6" "ii7/IV" "V7/IV" "IV" "viio6" "I" "V6" "V" "I" "IV" "viiø42" "V65/vi" "vi" "V42" "V" "V43/V" "V/V" "V" "I6" "V6/V" "V" "V6" "I" "V6" "viio7/vi" "IV65/vi" "vi" "ii65" "I6" "V" "I6" "I" }
         >>
    >>
  \layout {}
  \midi {
    \tempo 4 = 60 
  } 
}
      
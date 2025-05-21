# Falsehoods Programmers Believe About IBANs

In the spirit of [Falsehoods Programmers Believe About Phone Numbers](https://github.com/googlei18n/libphonenumber/blob/master/FALSEHOODS.md), here is a list of mistaken perspectives on International Bank Account Numbers (IBAN)...

1. **IBANs are global.**
   While the IBAN system has been deployed in some states on most continents, it is a long way from achieving universal adoption. Certain states, such as Australia, when their High Value Clearing Payments Association were queried regarding their decision not to adopt IBAN, first refused to respond for upwards of 12 months then finally refused to release any reasoning. There are a lot of established interests that are against reduced barriers to financial systems integration.

2. **IBAN country codes are the same as ISO3166-1 alpha-2 country codes.**
   Quite dangerously this is mostly, but not always the case. Both unofficial codes such as `AA` and various dependent territories which may use the parent jurisdiction's code instead of their own do not equate to the ISO3166-1 alpha-2 country code as expected.

3. **IBAN country codes are the same as IANA country codes.**
   Quite dangerously this is mostly, but not always the case. Take for example `XK`, unofficial codes such as `AA`, or various dependent territories which may use the parent jurisdiction's code instead of their own.

4. **IBAN represents a 'free' and 'neutral' namespace for global financial cooperation.**
   In fact, under the IBAN standard, which is managed by defacto global monopoly SWIFT, which has significant political significance to and affinity for US interests despite being a nominally Belgium-registered international cooperative, the only people who can create endpoints are existing financial institutions within countries holding an ISO3166-1 alpha-2 country code, a list which excludes many legitimate actors, virtually all innovators, plus the various states of the world with limited recognition. For a potentially mutually interoperable system adopted by some actors (eg. Bitcoin exchange Kraken) see the Internet IBAN (IIBAN) proposal at http://ifex-project.org/

5. **Pre-IBAN national checksums are still in operation.**
   There is no way to reliably determine whether or not a given country had a national, pre-IBAN checksum system, whether that system was actually applied to all banks (certain central banks are known exceptions), or whether that system is still in operation after IBAN adoption. The `php-iban` library represents a best-effort approach to gathering this knowledge as appropriate.

6. **IBAN is clearly published standard.**
   There are significant problems with the current dual-format publishing process used by SWIFT, which are documented [over here](https://raw.githubusercontent.com/globalcitizen/php-iban/master/docs/COMEDY-OF-ERRORS).

7. **IBANs are always written the same way.**
   Some countries tend to continue to use methods of spacing/delineation amongst legacy components present within the IBAN. Others tend to concatenate the entire IBAN to a machine-readable single word. Still others use the human-style formatting with four characters per block, `XXXX YYYY ZZZZ 0000`. It is difficult to know how to best present an IBAN to a customer. In general, reasonable practice is that if the user is likely to manually transcribe (eg. via pen and paper) then a human format (four characters per block) is recommended. If the output is likely to be copy-pasted, however, then a single word (machine format) is preferred... in which case care should be taken to exclude neighbouring punctuation.

8. **IBAN solves input errors.**
   IBAN has a strong checksum system built in, however this does not really help you to help the user find the source of an input problem. The `php-iban` library includes a flexible and robust mistranscription error detection system which can assist you in presenting possible errors to the user for manual evaluation.

9. **IBAN should be kept secret.**
   Bank account numbers are public identifiers and are generally not supposed to be secret. Any banking system capable of being exploited purely through the knowledge of an account number (The US check clearance system and early credit card systems were classic cases) should have long since been fixed by the time IBAN came in to being. The security argument in therefore misguided (see also, [security through obscurity](https://en.wikipedia.org/wiki/Security_through_obscurity)). However, for user experience optimization purposes it can be useful to obfuscate portions of an IBAN where it reduces cognitive load for users, such as in identifying one IBAN from a set using only the leading and unique trailing portions.

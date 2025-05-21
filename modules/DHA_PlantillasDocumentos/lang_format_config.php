<?php
/**
 * This file is part of Mail Merge Reports by Izertis.
 * Copyright (C) 2015 Izertis. 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Izertis at email address info@izertis.com.
 */
// Ver documentacion de la funcion "date" de PHP para el formato de fechas y horas
// Ver el programa Culture Explorer para ampliar a mas idiomas

$lang_format_config = array (

   'ca' => array (
      'week_days' => array("dilluns","dimarts","dimecres","dijous","divendres","dissapte","diumenge"),
      'months' => array("gener","febrer","març","abril","maig","juny","juliol","agost","setembre","octubre","novembre","desembre"),
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j \d\e F \d\e Y',  // 'j / F / Y'
      'bool_0' => 'No',
      'bool_1' => 'Si',
      'NumberToWords' => false,
      'CurrencyToWords' => false,
   ),

   'es' => array (
      'week_days' => array("lunes","martes","miércoles","jueves","viernes","sábado","domingo"),
      'months' => array("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"),
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j \d\e F \d\e Y', 
      'bool_0' => 'No',
      'bool_1' => 'Sí',
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),
   
   'es_MX' => array (
      'week_days' => array("lunes","martes","miércoles","jueves","viernes","sábado","domingo"),
      'months' => array("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"),
      'decimal_point' => '.',
      'thousands_sep' => ',',
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i a',
      'long_date_format' => 'j \d\e F \d\e Y', 
      'bool_0' => 'No',
      'bool_1' => 'Sí',
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ), 

   'es_AR' => array (
      'week_days' => array("lunes","martes","miércoles","jueves","viernes","sábado","domingo"),
      'months' => array("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"),
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i a',
      'long_date_format' => 'j \d\e F \d\e Y', 
      'bool_0' => 'No',
      'bool_1' => 'Sí',
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),   
   
   'en_US' => array (
      'week_days' => array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
      'months' => array("January","February","March","April","May","June","July","August","September","October","November","December"), 
      'decimal_point' => '.',
      'thousands_sep' => ',',
      'date_format' => 'm/d/Y',
      'time_format' => 'g:i A',
      'long_date_format' => 'F j, Y', 
      'bool_0' => 'No',
      'bool_1' => 'Yes',    
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),   
   
   'en_GB' => array (
      'week_days' => array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
      'months' => array("January","February","March","April","May","June","July","August","September","October","November","December"),      
      'decimal_point' => '.',
      'thousands_sep' => ',', 
      'date_format' => 'd/m/Y',
      'time_format' => 'H:i', 
      'long_date_format' => 'j F Y',
      'bool_0' => 'No',
      'bool_1' => 'Yes',  
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),      
   
   'en' => array (
      'week_days' => array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
      'months' => array("January","February","March","April","May","June","July","August","September","October","November","December"),      
      'decimal_point' => '.',
      'thousands_sep' => ',', 
      'date_format' => 'd/m/Y',
      'time_format' => 'H:i', 
      'long_date_format' => 'j F Y',
      'bool_0' => 'No',
      'bool_1' => 'Yes',  
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),      
   
   'de' => array (
      'week_days' => array("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"),
      'months' => array("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"),    
      'decimal_point' => ',',
      'thousands_sep' => '.',      
      'date_format' => 'd.m.Y',
      'time_format' => 'H:i',
      'long_date_format' => 'j. F Y',
      'bool_0' => 'Nein',
      'bool_1' => 'Ja',          
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),     

   'fr' => array (
      'week_days' => array("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
      'months' => array("janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"),      
      'decimal_point' => ',',
      'thousands_sep' => ' ',
      'date_format' => 'd/m/Y',
      'time_format' => 'H:i',   
      'long_date_format' => 'j F Y',
      'bool_0' => 'Non',
      'bool_1' => 'Oui',         
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),    
   
   'fr_BE' => array (
      'week_days' => array("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
      'months' => array("janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"),      
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd/m/Y',
      'time_format' => 'H:i',   
      'long_date_format' => 'j F Y',
      'bool_0' => 'Non',
      'bool_1' => 'Oui',         
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,      
   ),       
   
   'it_IT' => array (
      'week_days' => array("lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica"),
      'months' => array("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre"),  
      'decimal_point' => ',',
      'thousands_sep' => '.',      
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y',
      'bool_0' => 'No',
      'bool_1' => 'Sì',       
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,      
   ),    

   'pt_BR' => array (
      'week_days' => array("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira","sábado","domingo"),
      'months' => array("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro"),    
      'decimal_point' => ',',
      'thousands_sep' => '.',      
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j \d\e F \d\e Y',
      'bool_0' => 'Não',
      'bool_1' => 'Sim',        
      'NumberToWords' => false, //true,
      'CurrencyToWords' => true,      
   ),    
   
   'nl' => array (
      'week_days' => array("maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"),
      'months' => array("januari","februari","maart","april","mei","juni","juli","augustus","september","oktober","november","december"), 
      'decimal_point' => ',',
      'thousands_sep' => '.',      
      'date_format' => 'd-m-Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y',
      'bool_0' => 'Nee',
      'bool_1' => 'Ja',         
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,      
   ), 

   'dk' => array (
      'week_days' => array("mandag","tirsdag","onsdag","torsdag","fredag","lørdag","søndag"),
      'months' => array("januar","februar","marts","april","maj","juni","juli","august","september","oktober","november","december"),  
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd-m-Y',
      'time_format' => 'H:i',
      'long_date_format' => 'j. F Y',
      'bool_0' => 'Nej',
      'bool_1' => 'Ja',         
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),   

   'sv' => array (
      'week_days' => array("måndag","tisdag","onsdag","torsdag","fredag","lördag","söndag"),
      'months' => array("januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december"),   
      'decimal_point' => ',',
      'thousands_sep' => '.',      
      'date_format' => 'Y-m-d', 
      'time_format' => 'H:i',  
      'long_date_format' => 'j F Y',
      'bool_0' => 'Nej',
      'bool_1' => 'Ja',        
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,      
   ),   

   'pl' => array (
      'week_days' => array("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"),
      'months' => array("stycznia","lutego","marca","kwietnia","maja","czerwca","lipca","sierpnia","września","października","listopada","grudnia"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',      
      'date_format' => 'Y-m-d',
      'time_format' => 'H:i',
      'long_date_format' => 'j F Y',
      'bool_0' => 'Nie',
      'bool_1' => 'Tak',       
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),     
   
   'ru' => array ( 
      'week_days' => array("понедельник","вторник","среда","четверг","пятница","суббота", "воскресенье"),
      'months' => array("января","февраля","марта","апреля","мая","июня","июля","августа","сентября","октября","ноября","декабря"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',    
      'date_format' => 'd.m.Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y \г.',
      'bool_0' => 'нет',  // niet
      'bool_1' => 'да',   // da   
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),       
   
   'bg' => array (
      'week_days' => array("понеделник","вторник","сряда","четвъртък","петък","събота", "неделя"),
      'months' => array("Януари","Февруари","Март","Април","Май","Юни","Юли","Август","Септември","Октомври","Ноември","Декември"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',    
      'date_format' => 'd.m.Y \г.',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y \г.',
      'bool_0' => 'не',  // ne
      'bool_1' => 'да',  // da    
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,      
   ),        

   'id' => array (
      'week_days' => array("Senin","Selasa","Rabu","Kamis","Jumat","Sabtu", "Minggu"),
      'months' => array("Januari","Februari","Maret","April","Mei","Juni","Juli","Agustus","September","Oktober","November","Desember"),
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y', 
      'bool_0' => 'Tidak', 
      'bool_1' => 'Ya', 
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,
   ),  

   'tr_TR' => array (
      'week_days' => array("Pazartesi","Salı","Çarşamba","Perşembe","Cuma","Cumartesi","Pazar"),
      'months' => array("Ocak","Şubat","Mart","Nisan","Mayıs","Haziran","Temmuz","Ağustos","Eylül","Ekim","Kasım","Aralık"),
      'decimal_point' => ',',
      'thousands_sep' => '.',
      'date_format' => 'd.m.Y',
      'time_format' => 'G:i',
      'long_date_format' => 'j F Y', 
      'bool_0' => 'Hayir',
      'bool_1' => 'Evet',
      'NumberToWords' => false, //true,
      'CurrencyToWords' => true,
   ),  
   
   'he' => array (
      'week_days' => array("יום ראשון","יום שני","יום שלישי","יום רביעי","יום חמישי","יום שישי","שבת"),
      'months' => array("ינואר","פברואר","מרץ","אפריל","מאי","יוני","יולי","אוגוסט","ספטמבר","אוקטובר","נובמבר","דצמבר"),
      'decimal_point' => '.',
      'thousands_sep' => ',',   
      'date_format' => 'd/m/Y',
      'time_format' => 'G:i',   
      'long_date_format' => 'Y F j', 
      'bool_0' => 'לא',  // lo
      'bool_1' => 'אם',  // ken
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ),  
   
   'hu_HU' => array (
      'week_days' => array("hétfő","kedd","szerda","csütörtök","péntek","szombat","vasárnap"),
      'months' => array("január","február","március","április","május","június","július","augusztus","szeptember","október","november","december"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',   
      'date_format' => 'Y. m. d',
      'time_format' => 'G:i', 
      'long_date_format' => 'Y. F j.',  
      'bool_0' => 'Nem',
      'bool_1' => 'Igen',   
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false, //true,
   ), 

   'et' => array (
      'week_days' => array("esmaspäev","teisipäev","kolmapäev","neljapäev","reede","laupäev","pühapäev"),
      'months' => array("jaanuar","veebruar","märts","aprill","mai","juuni","juuli","august","september","oktoober","november","detsember"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',    
      'date_format' => 'd.m.Y',
      'time_format' => 'G:i', 
      'long_date_format' => 'j. F Y. a.',      
      'bool_0' => 'Ei',
      'bool_1' => 'Jah',     
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,
   ), 

   'cs' => array (
      'week_days' => array("pondělí","úterý","středa","čtvrtek","pátek","sobota","neděle"),
      'months' => array("leden","únor","březen","duben","květen","červen","červenec","srpen","září","říjen","listopad","prosinec"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',      
      'date_format' => 'd.m.Y',
      'time_format' => 'H:i', 
      'long_date_format' => 'j. F Y',      
      'bool_0' => 'Ne',
      'bool_1' => 'Ano ',     
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,
   ),  

   'lt' => array (
      'week_days' => array("pirmadienis","antradienis","trečiadienis","ketvirtadienis","penktadienis","šeštadienis","sekmadienis"), 
      'months' => array("sausis","vasaris","kovas","balandis","gegužė","birželis","liepa","rugpjūtis","rugsėjis","spalis","lapkritis","gruodis"),
      'decimal_point' => ',',
      'thousands_sep' => '.', 
      'date_format' => 'Y.m.d',
      'time_format' => 'G:i',   
      'long_date_format' => 'Y \m. F j \d.',       
      'bool_0' => 'Ne',
      'bool_1' => 'Taip',   
      'NumberToWords' => false, //true,
      'CurrencyToWords' => false,
   ),

   'sk_SK' => array (
      'week_days' => array("pondelok","uterok","streda","štvrtok","piatok","sobota","nedeľa"),
      'months' => array("január","február","marec","apríl","máj","jún","júl","august","september","október","november","december"),
      'decimal_point' => ',',
      'thousands_sep' => ' ',      
      'date_format' => 'd.m.Y',
      'time_format' => 'H:i', 
      'long_date_format' => 'j. F Y',      
      'bool_0' => 'Nie',
      'bool_1' => 'Áno ',     
      'NumberToWords' => false,
      'CurrencyToWords' => false,
   ),      
   
);
?>
# Formularios Web Avanzados #
## Introducción ##
Los formularios web son uno de los canales habituales a través de los que una entidad recibe información de su base social: donativos, altas de socios, recogidas de firmas, suscripciones a boletines electrónicos, inscripciones a eventos, peticiones de servicios, etc.

Que la información recibida por esta vía quede directamente incorporada al CRM de la entidad supone unos beneficios notables, puesto que evita cualquier proceso de traspaso o de registro manual de los datos recibidos y mejora la calidad de la información que entra en la aplicación.

El sistema de **Formularios Web Avanzados** ofrece una herramienta integral, altamente versátil, reutilizable y flexible. El objetivo de este sistema es permitir la captura de información compleja desde el exterior y automatizar flujos de trabajo de forma autónoma, ágil y sin necesidad de desarrollos a medida.

## Escenarios de Uso y Beneficios Operativos
El sistema de Formularios Web Avanzados permite configurar la captación de datos y automatizar su registro en el CRM. A continuación, se detallan los principales escenarios de uso:

### Captación y automatización de procesos en un solo paso ###
* **El escenario:** La entidad necesita publicar un formulario para captar nuevo voluntariado o sumar personas a su base social. Al recibir los datos, se requiere registrar a la persona en el CRM, dejar constancia de su tipo de vinculación con la entidad, incluirla en una lista de distribución para futuras comunicaciones, enviarle un mensaje de bienvenida y notificar al equipo encargado.
* **La aportación del sistema:** Permite configurar un formulario que encadena todas estas acciones. Al enviarse, el sistema ejecuta secuencialmente:
    1. Crea o actualiza el registro en el módulo de Personas.
    2. Crea un registro en el módulo de Relaciones con Personas indicando que el "Tipo de relación" con la entidad es de "Voluntario/a".
    3. Relaciona ambos registros entre sí.
    4. Añade a la persona a la Lista de Público Objetivo (LPO) predefinida.
    5. Dispara el envío del correo electrónico de bienvenida a la persona inscrita.
    6. Envía un correo de notificación al equipo responsable en el CRM.

### Universalidad de módulos y operativas a medida ###
* **El escenario:** Las entidades gestionan operativas muy diversas y a menudo utilizan módulos personalizados (como "Peticiones de Material", "Adopciones", "Incidencias" o "Proyectos") que requieren capturar información desde el exterior.
* **La aportación del sistema:** Permite construir formularios sobre **cualquier módulo del CRM**, incluyendo los creados a medida por la entidad desde el área de administración (Estudio). Además, facilita recoger datos de varios módulos a la vez (por ejemplo, permitir que un visitante registre una "Incidencia" y que el sistema la vincule automáticamente a un "Centro" y a la "Persona" que informa).

### Crecimiento orgánico de la base de datos ###
* **El escenario:** Recibir nueva información desde la web supone el reto de mantener el histórico de los contactos intacto, especialmente cuando las personas comparten datos (como familiares con un mismo correo electrónico) o dejan campos clave en blanco.
* **La aportación del sistema:** Incorpora un sistema de **detección de duplicados en cascada**. Permite definir reglas de coincidencia de forma independiente para cada tipo de registro (ej. buscar a la persona por DNI, y si no lo ha introducido, buscarla por Email). La acción **"Ampliar"** rellena solo los campos vacíos del CRM respetando los datos existentes.

### Mejora de la participación en campañas ###
* **El escenario:** Al enviar comunicaciones masivas por correo (ej. boletines para captar firmas, apoyo o actualización de datos), si la persona llega a un formulario completamente vacío, existe el riesgo de que abandone el proceso por tener que rellenar información que la entidad ya posee.
* **La aportación del sistema:** Permite el pre-rellenado mediante URL. Cuando la persona hace clic en el correo, aterriza en el formulario con sus datos personales (nombre, email, etc.) ya cumplimentados, facilitando la participación.

### Comunicaciones automáticas enriquecidas ###
* **El escenario:** Tras rellenar una solicitud o inscripción, es necesario enviar una confirmación a la persona inscrita con los detalles de su petición.
* **La aportación del sistema:** El equipo técnico de la entidad puede diseñar plantillas de correo electrónico corporativas y asignarlas al formulario. El sistema envía automáticamente estas plantillas como respuesta inmediata, inyectando de forma dinámica la información de los registros que se acaban de crear. Así, la persona recibe un correo con un formato atractivo que incluye sus datos personales y los detalles específicos de la solicitud o evento, cuyo contenido y estética define la entidad a medida.

### Uso como interfaz interna para operaciones complejas ###
* **El escenario:** En el trabajo diario, el equipo a menudo necesita dar de alta registros relacionados navegando por diferentes pantallas del CRM (ej. crear una persona, luego crear su organización, y finalmente vincularlos).
* **La aportación del sistema:** Los formularios también pueden usarse de forma interna como herramientas de entrada de datos. Se puede diseñar un formulario avanzado y restringirlo por seguridad para que solo sea accesible por el personal con una sesión iniciada en el CRM y con los permisos de rol adecuados para la creación en todos los módulos implicados. Así, el equipo dispone de una pantalla única para introducir datos complejos, quedando la autoría del registro asignada automáticamente a la persona usuaria responsable. La URL generada del formulario puede añadirse como un enlace directo en el menú principal de navegación del CRM.

### Escalabilidad tecnológica y arquitectura abierta ###
* **El escenario:** Las entidades desarrollan reglas de negocio específicas o necesitan integraciones particulares.
* **La aportación del sistema:** Cuenta con una arquitectura modular y desacoplada del CRM. Permite a los desarrolladores programar **nuevas acciones y automatismos a medida** mediante código. Estos nuevos componentes se integran y aparecen en el asistente visual, permitiendo al equipo utilizarlos en cualquier formulario.

### Despliegue y autonomía técnica ###
* **El escenario:** Se necesita publicar un formulario con la imagen corporativa, disponer de él rápidamente o reutilizar campañas pasadas asegurando que sea accesible para todo el mundo.
* **La aportación del sistema:** Los formularios se guardan en el sistema, lo que permite editarlos, duplicarlos y reutilizarlos. El sistema incluye un editor visual para adaptar la estética, colores, tipografías y logotipos. El resultado es un **formulario responsivo y accesible** (basado en Bootstrap 5 y etiquetas semánticas ARIA) que garantiza una experiencia de usuario óptima en diferentes dispositivos (móviles, tabletas o pantallas de escritorio) y permite la navegación mediante tecnologías de asistencia (como lectores de pantalla). El propio CRM puede generar una URL pública y alojar el formulario, o proporcionar el código HTML o Iframe para incrustarlo en una web externa.

### Auditoría y trazabilidad total ###
* **El escenario:** Se requiere auditar qué registros del CRM fueron alterados por un envío concreto de un formulario web o analizar estadísticamente las respuestas.
* **La aportación del sistema:** Registra cada respuesta recibida en la base de datos. A través del módulo de "Vínculos", proporciona una trazabilidad que permite auditar qué información introdujo el visitante y qué acciones exactas desencadenó (qué registros concretos se crearon, actualizaron o ignoraron). Además, incorpora un componente específico para el análisis estadístico y la extracción de métricas de las respuestas recogidas.


## Seguridad e Integridad de los Datos
Los Formularios Web Avanzados incorporan funcionalidades de seguridad nativas para proteger el CRM de ataques, correo basura y preservar la calidad de la información.

### Protección Anti-Spam Integrada e Invisible ###
El sistema bloquea ataques automatizados mediante filtros invisibles, sin depender de servicios de terceros ni requerir interacción adicional por parte del visitante:
* **Campo trampa (Honeypot):** Se inyecta automáticamente un campo oculto en el diseño del formulario que los humanos no ven. Si un bot lo rellena, el sistema detecta el engaño y descarta el envío.
* **Control de tiempo (TimeTrap):** El sistema monitoriza el tiempo de respuesta y bloquea automáticamente cualquier envío realizado de forma antinatural (en menos de 2 segundos).
* **Filtro de firmas (User-Agent):** Se bloquean las peticiones provenientes de herramientas de programación utilizadas habitualmente para lanzar ataques masivos analizando la firma tecnológica del navegador (User-Agent) en el momento del envío.

### Validación de Datos a Doble Capa y a Medida ###
Para asegurar que la información que entra es válida (ej. que un correo está bien escrito, o un DNI o teléfono tienen el formato correcto), el sistema verifica los datos por partida doble:
* **En el navegador:** Avisa a la persona rápidamente si hay un error de formato antes de enviar los datos.
* **En el servidor (CRM):** Antes de guardar la información, el sistema re-verifica los datos de forma estricta para asegurar el cumplimiento de las reglas de validación.
* **Validadores extensibles:** La arquitectura del sistema permite a los desarrolladores **programar validadores personalizados por código** para cubrir requisitos de calidad de datos exclusivos de la entidad, sumándose estos al catálogo estándar.

### Salvaguardas de Integridad ante Duplicados ###
El sistema evalúa los campos configurados y aplica estrategias de resolución de duplicados.

Como medida de seguridad arquitectónica, **si un formulario llega con un campo clave en blanco, el sistema ignora esa regla de duplicidad**. Esto evita falsos positivos (como que el CRM fusione a dos personas distintas en una sola ficha, solo porque ninguna de las dos indicó su correo electrónico al rellenar el formulario).

## Conceptos Clave ##
Antes de crear el primer formulario, es útil conocer cómo se organiza la información en el sistema de Formularios Web Avanzados:
* **Bloque de datos**: Es el corazón del formulario. Representa una unidad de información que se recogerá y se guardará como un único registro en un módulo del CRM (por ejemplo, una "Persona" o una "Inscripción"). Se pueden incluir varios bloques en un mismo formulario, aplicando lógicas distintas a cada uno (por ejemplo, un bloque para el "tutor" y otro para el "menor", ambos registros del módulo "Persona").

* **Acciones**: Son automatismos que el sistema ejecuta con los datos recogidos. Permiten validar campos, crear o actualizar los registros en el CRM, enviar correos electrónicos personalizados o redirigir al usuario a otra página web.

* **Flujo de acciones**: Es la secuencia ordenada en la que se ejecutan las acciones. Principalmente existe un "flujo principal" (si todo va bien) y un "flujo de tratamiento de errores" (si algo falla).

* **Secciones**: Son contenedores visuales que agrupan los campos del formulario para facilitar su usabilidad. Pueden tener distintos formatos, como paneles simples, tarjetas y ser desplegables. Las secciones no afectan a los datos, solo a cómo se ven en el formulario.

## ¿Cómo crear un Formulario?: El Asistente Paso a Paso ##
La creación y configuración de un formulario se realiza de forma guiada a través de un asistente visual (wizard) dividido en 5 pasos:

### Paso 1: Información general ###
En este primer paso se definirá la identidad del formulario. Se podrán indicar los datos básicos como su Nombre, la **persona asignada** (quien, además de ser responsable del formulario, será a quien se asignen por defecto los nuevos registros creados si el envío es externo) y la descripción.

### Paso 2: Estructura y campos ###
Aquí se decidirá qué información se quiere pedir a los usuarios. La selección de esta información se realiza mediante la adición de bloques de datos, que pueden ser de dos tipos principales dependiendo de su integración con el sistema:

* **Bloques de datos (enlazados)**: Son unidades de información que están conectadas directamente a un módulo específico del CRM (por ejemplo, "Personas", "Inscripciones" u "Organizaciones"). El propósito de estos bloques es que los datos recopilados sirvan para **crear o actualizar un registro real** dentro del sistema. Al estar enlazados a un módulo, permiten definir validaciones propias del CRM, configurar reglas para la detección y gestión de duplicados, y establecer campos de servidor u ocultos.

* **Bloques de datos no enlazados**: Son contenedores diseñados para recopilar información en el formulario, pero que **no están vinculados a ningún módulo del CRM**. Como consecuencia, los datos introducidos quedarán guardados de forma independiente y exclusiva dentro del registro global de la "Respuesta". **Importante: Esta información no se volcará en la ficha de la Persona ni alterará ningún registro del CRM.** A diferencia de los bloques enlazados (que permiten mezclar campos del CRM con campos virtuales), un bloque no enlazado solo puede contener campos no enlazados. Son ideales para realizar encuestas, recopilar información temporal, hacer valoraciones o incluir casillas de aceptación de condiciones.


#### Secciones de los Bloques de datos ####
En el asistente, los bloques de datos están definidos en distintas secciones. Estas secciones pueden ser distintas según el tipo de bloque (enlazado o no enlazado).

##### Campos #####
Esta sección aparece en ambos bloques, enlazados y no enlazados

Aquí se define qué datos componen el bloque. Al configurar los campos, se organiza la información en dos pestañas principales:

* **Formulario**: Son los campos visibles que el visitante podrá rellenar. Por cada campo se puede configurar su etiqueta (nombre visible), si es obligatorio, el tipo de entrada (texto, desplegable, fecha, etc.) y el texto de fondo (*placeholder*).
  * **Buena práctica de configuración:** Es altamente recomendable establecer siempre **al menos un campo como obligatorio** en cada bloque de datos enlazado (por ejemplo, el Nombre o el Email). Si todos los campos de un bloque se configuran como opcionales y el visitante los deja en blanco, el sistema procesará el bloque igualmente y creará un registro vacío en el CRM.

  * **Textos de ayuda y Enlaces**: Cada campo permite añadir un texto de ayuda o descripción para guiar al usuario. Además, el asistente incluye una herramienta específica para insertar fácilmente enlaces a páginas externas (ideales para acompañar a las casillas de aceptación de Políticas de Privacidad o Condiciones de Uso).

  * **Campos no enlazados (virtuales)**: Son campos que no alteran la base de datos principal, sino que su valor vive exclusivamente en el registro de la "Respuesta" enviada (es decir, **no se guardarán en el perfil de la persona ni en los módulos del CRM**). Para agilizar la creación de formularios (especialmente cuando se configuran opciones personalizadas o controles de encuestas), el asistente permite duplicar estos campos con un solo clic, generando una copia exacta de toda su configuración. Existen en dos contextos:
    1. **Dentro de un Bloque no enlazado**: Donde, por la naturaleza del bloque, todos los campos creados son obligatoriamente de este tipo.
    2. **Dentro de un Bloque enlazado**: Convivirán junto a los campos normales del CRM. Posibles usos: Son ideales para recabar información que solo tiene sentido en el contexto del envío (como una casilla de "Acepto las condiciones", comentarios adicionales o valoraciones temporales) evitando que estos datos "ensucien" la ficha de la Persona o Inscripción en el CRM.
  
  * **Validaciones**: Adicionalmente, se pueden vincular acciones de validación a campos específicos para garantizar la calidad de los datos introducidos. **Para agilizar la configuración, el sistema asigna validadores automáticamente** al detectar ciertos tipos de campos (por ejemplo, al añadir un campo de email, su validador se vincula solo). El sistema incluye un **amplio catálogo de validadores predefinidos**:

    * Formato de Email.
    * Documentos de identidad y salud: DNI, NIE, CIF, NUSS (Número de Afiliación a la Seguridad Social) y CIP (Código de Identificación Personal de CatSalut).
    * Datos bancarios y de contacto: IBAN y Teléfono (con validación de longitud para España o prefijos internacionales).
    * Ubicación: Código Postal (formato de España).
    * Límites y rangos: Comprobación de Edad (mínima y máxima a partir de una fecha de nacimiento), límites numéricos (valor mínimo/máximo) y longitud de texto (número mínimo y/o máximo de caracteres).
    * Otros: Direcciones web (URL), casillas de marcación obligatoria (indispensable para aceptar términos y condiciones), y validación libre mediante expresiones regulares (Regex).


  * **Condicionalidad de las validaciones**: Se pueden configurar reglas simples para que un validador solo se ejecute si otro campo del formulario contiene un valor específico (por ejemplo, validar el campo "número de identificación" como "NIE" solo si el campo "Tipo de identificación" es "nie", o si una casilla específica está marcada). También es posible personalizar el mensaje de error exacto que verá el usuario si la validación falla.

* **Servidor (solo en bloques enlazados)**: Contiene los **campos de servidor**, que son valores constantes y ocultos que no se muestran al usuario final, pero que el sistema guardará en el CRM al crear el registro. Por ejemplo, en una inscripción, puedes definir por defecto el estado de la inscripción como "Confirmado" o vincularlo a un "Evento" específico sin que esta información esté presente en el formulario que el visitante rellenará.

  * **Fechas relativas**: En el caso de los campos de tipo fecha, el sistema permite configurar valores dinámicos relativos al momento del envío: *Hoy*, *Ahora*, *Dentro de un día*, *Dentro de una semana*, *Dentro de un mes*, *Último día de este mes* o *Primer día del próximo mes*. También es posible utilizar expresiones en inglés (como '*+2 weeks*', o '*first Monday of next month*'). El sistema calculará la fecha exacta partiendo del momento en que se reciba la respuesta.

* **Selección de los campos a utilizar**: Al añadir un campo que representa un valor en el registro (ya sea un campo normal del "**Formulario**" o un campo de "**Servidor**"), el listado de campos disponibles mostrará por defecto únicamente aquellos que son visibles en la vista de edición o detalle del propio módulo en el CRM. Sin embargo, si se activa la opción *"Mostrar todos los campos"*, el listado revelará la totalidad de los campos del módulo. Esto resulta de gran utilidad si se desea operar con campos que se han ocultado expresamente desde Estudio (por ejemplo, los campos del bloque "Incorpora" en la ficha de Personas).

* **Gestión avanzada de opciones y campos booleanos**: Cuando un campo requiere que el usuario escoja entre distintos valores (al seleccionar el Tipo de entrada *"Opciones predeterminadas"*), el sistema ofrece un alto nivel de configuración:

  * **Campos con opciones (Desplegables/Selección de opción)**: El sistema cargará los valores del CRM por defecto. Si se activa la opción de *"Personalizar opciones"*, es posible modificar el comportamiento de esta lista en el formulario: permite cambiar el orden y el texto visible (nombre) de los elementos, y gestionar su visibilidad para ocultar aquellos elementos que no se deseen ofrecer en el formulario.

  * **Campos relacionados**: Si el campo es del tipo Relacionado (apunta a un registro de otro módulo del CRM), se podrá seleccionar y definir explícitamente qué registros exactos (elementos) aparecerán listados para que el usuario seleccione uno de ellos. Esto es útil para permitir seleccionar en el formulario el curso (Evento) al que se inscribirá de entre los activos, por ejemplo.

  * **Campos no enlazados**: Al no depender de la estructura del CRM, en los campos virtuales o no enlazados se puede definir desde cero el listado completo de todas las opciones predeterminadas.
  
  * **Campos booleanos (Sí/No)**: Si el campo del CRM es de tipo Booleano, la herramienta permite elegir cómo representarlo gráficamente: puede mostrarse como un *Desplegable* con las opciones de "Sí" y "No", como una *Casilla de selección (Checkbox)* única, o bien como un *Interruptor (Switch)* visual.

###### Tipos de entrada y Editores visuales (Subtipos) ######
El sistema adapta el tipo de control visual en el formulario en función del tipo de dato, permitiendo decidir cómo se presenta la información de forma precisa. Los grandes **Tipos de entrada** dictan el comportamiento general, y para cada uno existen diferentes **Tipos de editor (Subtipos)** para afinar la experiencia de usuario:

* **➖ Texto**: Entradas de una sola línea.
  * **🔤 Texto simple**: Campo de texto estándar.
  * **✉️ Correo electrónico**: Valida el formato email y habilita teclados específicos en dispositivos móviles.
  * **📞 Teléfono**: Habilita el teclado numérico telefónico en dispositivos móviles.
  * **🔗 Dirección URL**: Formato específico para enlaces web.
  * **🔒 Contraseña**: Oculta los caracteres a medida que el usuario escribe.

* **☰ Texto largo**: Áreas de texto para descripciones extensas.
  * **📃 Párrafo**: Área de texto ampliada (Textarea).
    
* **#️⃣ Numérico**: Entradas restringidas a números.
  * **🔢 Numérico**: Control estándar de entrada numérica.

* **🗓️ Selección de tiempo**: Controles nativos para escoger fechas u horas.
  * **📅 Fecha**: Selector de día, mes y año.
  * **⏱️ Hora**: Selector de hora y minutos.
  * **📅⏱️ Fecha y hora**: Selector combinado.
  
* **▼ Opciones predeterminadas**: Muestra un conjunto cerrado de opciones entre las que el visitante debe elegir.
  * **🔻 Desplegable**: Clásica lista de selección colapsada (Combo box).
  * **📑 Desplegable múltiple**: Lista de selección que permite escoger más de una opción a la vez.
  * **🔘 Selección de opción**: Muestra todas las opciones directamente en pantalla con botones de radio (*Radio buttons*), permitiendo escoger solo una.
  * **☑️ Selección múltiple**: Muestra casillas de verificación (*Checkboxes*) independientes para seleccionar varias opciones simultáneamente.
  * **◻️ Casilla de selección**: Un único *Checkbox* simple (ideal para booleanos o aceptaciones de términos).
  * **🎚️ Interruptor**: Un control visual estilo "Switch" (encendido/apagado para booleanos).

* **🏅 Valoración (Solo en campos no enlazados)**: Controles visuales pensados para encuestas y recolección de *feedback*. Exclusivos de los campos no enlazados, guardan internamente su valor como un número entero.
    *   **⭐ Estrellas**: Valoración clásica del 1 al 5.
    *   **🙂 Caras**: Valoración del 1 al 5 a través de Emojis.
    *   **🚦 Semáforo**: Valoración rápida de 3 niveles (rojo, amarillo, verde).
    *   **👍 Pulgares**: Valoración binaria de aprobación o rechazo (arriba/abajo).
    *   **🔟 Escala 0-10 (NPS)**: Barra estándar de escala *Net Promoter Score*.

* **🕵️ Oculto**: El campo no se muestra gráficamente en el formulario, pero su valor se almacena y acompaña silenciosamente a la respuesta enviada.

##### Detección de duplicados ##### 
*(Esta sección solo aparece en los bloques enlazados)*

El sistema permite definir reglas precisas para detectar si el registro (persona, organización, inscripción, etc.) ya existe en el CRM antes de crearlo. Destacan dos características fundamentales en su funcionamiento:

* **Reglas múltiples y secuenciales**: Es posible configurar más de una regla de detección para un mismo bloque. El sistema las evaluará en el orden en que se hayan definido. En el momento en que una regla detecte una coincidencia, el sistema aplicará la acción configurada para esa regla y dejará de evaluar las siguientes. **Importante:** Las reglas son totalmente independientes. Si una regla falla y el sistema evalúa la siguiente, lo hará fijándose *únicamente* en los campos de esta nueva regla (ignorando si hay discrepancias en otros campos del formulario).

* **Salvaguarda de campos vacíos**: Para evitar falsos positivos (por ejemplo, que el sistema asuma que dos personas son la misma solo porque ambas dejaron el campo *Email* en blanco), el sistema ignorará automáticamente cualquier regla si el visitante no ha rellenado todos los campos implicados en ella durante el envío del formulario.

*Casos de uso y estrategia*: Esta flexibilidad permite crear búsquedas en cascada muy robustas, pero deben diseñarse cuidadosamente para evitar fusiones indeseadas. Un ejemplo seguro para un bloque de "Persona" sería:
1. **Regla 1 (Email + DNI)** -> Acción: *Actualizar*. (Si el visitante indica ambos datos y coinciden con la base de datos, es indudablemente la misma persona: se sobrescribe su ficha con los datos más recientes).

2. **Regla 2 (Email)** -> Acción: *Ampliar* o *Ignorar*. (Si la regla 1 ha fallado, por ejemplo, porque el usuario no indicó su DNI, el sistema buscará solo por Email. Al encontrarlo, la acción 'Ampliar' asegura que solo se rellenarán los datos que estuvieran vacíos en el CRM, evitando sobrescribir información crítica en caso de que dos familiares compartan el mismo correo electrónico).

Por cada regla definida, se debe elegir entre 4 comportamientos si se detecta que el registro ya existe:

* **Actualizar (Sobrescribir)**: Reemplaza los datos existentes en el CRM por los nuevos datos introducidos en el formulario.

* **Ampliar**: Añade la información nueva del formulario solo en aquellos campos que en el CRM estuvieran vacíos, respetando los datos que ya existían.

* **Ignorar**: Ignora por completo los datos recibidos en ese bloque para no modificar el registro original del CRM.

* **Error**: Detiene el procesado y genera un error (útil cuando un formulario solo debería aceptar registros estrictamente nuevos, como el alta de una nueva sede).


##### Relaciones ##### 
*(Esta sección solo aparece en los bloques enlazados)*

Esta sección es fundamental cuando el formulario interactúa con más de un módulo del CRM. Sirve para definir cómo se conectan los registros generados entre sí dentro de la base de datos.

* Al definir una relación, se seleccionará el tipo de vínculo exacto y el bloque de datos destino de la relación. Al hacerlo, el sistema creará automáticamente las acciones "por detrás" para enlazar ambos registros una vez se guarde la respuesta.

* Ejemplo práctico: Imaginemos un formulario de inscripción de una persona a un evento. En esta sección de Relaciones se indicará que ambos bloques (Persona e Inscripción) están unidos. Así, cuando el usuario envíe el formulario, el sistema no solo creará los dos registros por separado, sino que automáticamente generará el vínculo formal entre ambos dentro del CRM.


### Paso 3: Lógica y automatismos ###
En este paso se configurará qué ocurre "por detrás" cuando alguien hace clic en "Enviar" del formulario. La lógica de negocio se articula mediante un sistema visual de flujos compuesto por **Acciones**.

* **Dos flujos de ejecución**: Cada formulario se organiza visualmente en pestañas que representan distintos flujos de ejecución. Principalmente, existen dos:

  * **Flujo Principal**: Es la lista ordenada de acciones que se ejecutan de forma secuencial cuando se procesa correctamente una respuesta.

  * **Flujo de Error**: Es un flujo de contingencia que se ejecuta de forma automática si alguna de las acciones del flujo principal falla.

* **Acciones automáticas (Persistencia garantizada)**: Al añadir bloques de datos en el paso anterior del asistente, el sistema añade **automáticamente** al flujo principal las acciones necesarias para guardar los datos en el CRM ("Guardar registro") y vincularlos entre sí ("Enlazar registros"). Gracias a esto, no es necesario preocuparse por la persistencia de la información; el sistema garantiza que los registros se crearán, actualizarán y relacionarán solos según las reglas definidas. Estas acciones base no se pueden eliminar, pero sí reordenar.

  * **Optimización de relaciones 1-N (Guardar registro con relaciones)**: Cuando se define una relación de tipo 1-N (un bloque "padre" y un bloque "hijo" que contiene un campo FK apuntando al padre), el sistema utiliza una acción especial **"Guardar registro con relaciones"** en lugar de la combinación tradicional de "Guardar registro" + "Enlazar registros". Esta acción inyecta el ID del registro padre en el campo FK del registro hijo **antes** del primer guardado, evitando un paso de actualización posterior innecesario. Las relaciones N-M siguen utilizando la acción "Enlazar registros", creando una única acción (no dos) ya que la relación es bidireccional.

* **Acciones definidas por el usuario**: Más allá de guardar los datos, se pueden enriquecer los flujos añadiendo y encadenando nuevas acciones configurables. Cada acción introducida permite establecer parámetros específicos basándose en los propios datos introducidos en el formulario o en registros del CRM.

  * **Condiciones de ejecución**: La ejecución de cualquier acción se puede condicionar a los datos introducidos por el usuario. Por ejemplo, se puede configurar que la acción de "Añadir a LPO" (suscripción a newsletter) solo se ejecute si el usuario ha marcado previamente la casilla de "Deseo recibir información" en el formulario.

  * **Tolerancia a fallos (Continuar en caso de error)**: En la configuración de cada acción se puede habilitar esta opción de seguridad. Si se activa y la acción falla de forma aislada (por ejemplo, una caída temporal del servidor de correo al enviar una notificación), el sistema ignorará el fallo, no saltará al flujo de error y permitirá que el resto del formulario termine de procesarse con éxito.


* **Acciones finales**: Es clave destacar un tipo especial de acciones llamadas "Finales". Una acción final es aquella que, una vez ejecutada, **pierde el control del proceso y finaliza el flujo de acciones por completo**, impidiendo que se ejecute ninguna otra acción posterior. Se utilizan para operaciones de cierre, como redirigir al usuario a una página web externa o mostrar un mensaje final.

* **Un sistema escalable y ampliable**: La arquitectura de este sistema de acciones está diseñada exclusivamente por código y de forma totalmente desacoplada del "core" del CRM. Esto significa que el ecosistema de acciones es **fácilmente ampliable**: permite desarrollar a medida nuevas acciones o integraciones para entidades concretas y sumarlas al catálogo general.

* **Orden de ejecución de las acciones**: El sistema ordena las acciones automáticamente mediante un orden topológico que respeta las dependencias entre ellas. En particular, cuando existe una relación 1-N, el bloque padre (lado 1) se guarda **siempre antes** que el bloque hijo (lado N), ya que el hijo necesita el ID del padre para inyectarlo en su campo FK. Las relaciones N-M se resuelven mediante una única acción "Enlazar registros" que se sitúa tras los guardados. **Nota importante para administradores avanzados:** Los Flujos de Trabajo y LogicHooks que se configuren en los módulos implicados se ejecutarán durante el guardado inicial del registro, momento en el cual las relaciones N-M con otros registros del formulario aún no se habrán creado (las relaciones 1-N, en cambio, ya están resueltas porque el FK se inyecta antes del guardado). Si un Workflow necesita consultar una relación N-M con otro registro creado en el mismo formulario, deberá configurarse para ejecutarse en un paso posterior al guardado inicial.

#### Catálogo actual de Acciones ####
Inicialmente, el sistema incluye las siguientes acciones que se pueden añadir a los flujos:
* **Guardar registro (Automática)**: Crea o actualiza un registro a partir de un bloque de datos. Cuenta con un sistema de **asignación inteligente**: si el formulario se rellena con una sesión activa en el CRM, el registro se asignará a dicho trabajador. Si es un envío externo, se asignará a la persona responsable del formulario (o al administrador del sistema en su defecto).

* **Guardar registro con relaciones (Automática)**: Variante optimizada de "Guardar registro" que, además de crear o actualizar el registro, inyecta los valores FK necesarios para las relaciones 1-N salientes en un solo guardado. Se genera automáticamente cuando un bloque de datos tiene relaciones 1-N con otros bloques. No es seleccionable por el usuario.

* **Enlazar registros (Automática)**: Enlaza dos registros del CRM según las relaciones definidas en el paso anterior. Para relaciones N-M se genera una única acción (la relación es bidireccional). Para relaciones 1-N, esta acción solo se genera cuando la FK no puede inyectarse en el guardado inicial (por ejemplo, si el bloque no dispone del campo FK).

* **Enviar notificación por correo / al usuario asignado**: Permite enviar emails automatizados utilizando las plantillas del CRM. Esta acción cubre dos grandes escenarios:
  * *Correos de respuesta al usuario (Auto-respondedor)*: Se puede enviar un correo de confirmación, agradecimiento o resguardo directamente a la persona visitante que ha rellenado el formulario (leyendo el email del registro recién creado).
  * *Avisos internos*: Se puede notificar a una dirección interna o al trabajador responsable del registro asignado.
  * **Novedad destacada:** En ambos casos, el sistema procesa la plantilla inyectando la información de **todos los registros implicados en la respuesta**. Es decir, si un formulario crea a la vez una "Persona" y una "Inscripción", la plantilla de correo podrá interpretar dinámicamente variables de ambos módulos simultáneamente (siempre que sean módulos distintos), dotando a las comunicaciones de un contexto mucho más rico sin configuraciones adicionales.


* **Añadir a LPO**: Añade el registro resultante a una Lista de Público Objetivo destino.

* **Verificar sesión activa y permisos**: Bloquea el acceso y el procesamiento del formulario si no hay una sesión activa en el CRM o si el usuario no dispone de permisos explícitos de edición/creación para **todos y cada uno de los módulos** involucrados en los bloques de datos del formulario.

* **Mostrar página con resumen de respuestas (Final)**: Redirige al usuario a una página que contiene el resumen de todas sus respuestas al formulario.

* **Redireccionar a página (Final)**: Redirige al usuario a una página web externa una vez procesados los datos. Esta acción permite adjuntar datos recopilados en el formulario y enviarlos a la URL de destino (utilizando los métodos estándar GET o POST).

#### Acciones Diferidas (Procesos en espera de eventos externos) ####
Más allá de las acciones inmediatas, el sistema incorpora un potente motor de **acciones diferidas** que permite pausar temporalmente el procesamiento de una respuesta y esperar a que ocurra un evento externo antes de continuar. Este mecanismo resulta esencial para escenarios que requieren interacción asíncrona con servicios o personas externas al CRM:

* **Confirmación de correo electrónico**: Detiene el flujo de procesamiento y envía un correo con un enlace único al visitante. El sistema espera hasta que la persona hace clic en ese enlace para verificar su dirección de correo, momento en que se reanuda automáticamente el flujo y se continúa con el resto de acciones configuradas.

* **Pasarelas de pago**: Permite integrar pagos en línea dentro del flujo del formulario. Cuando se alcanza una acción de pago, el sistema redirige al usuario a la pasarela bancaria (TPV) y se queda a la espera de la confirmación del cobro, que llega de forma asíncrona mediante un webhook. Una vez confirmado (o rechazado), el flujo se reanuda por el camino de éxito o de error configurado, permitiendo enviar correos de confirmación de pago, generar recibos o actualizar el estado del compromiso de pago. Las pasarelas soportadas incluyen: **Redsys** (TPV y Bizum), **Stripe** (Checkout), **PayPal** y **CECA**.

* **Procesamiento asíncrono de respuestas**: El sistema permite configurar el modo de procesamiento entre síncrono y asíncrono. En modo asíncrono, las respuestas se guardan de forma inmediata y se procesan en segundo plano mediante la cola de trabajos del CRM, evitando sobrecargas del servidor en formularios con picos de uso muy altos.

### Paso 4: Maquetación ###
Llega el momento de darle forma visual al formulario, separando por completo su diseño de la lógica estructural definida en los pasos anteriores. Este paso cuenta con un **editor visual con previsualización en tiempo real**, lo que permite comprobar exactamente cómo quedará el formulario final a medida que se diseña y se aplican los cambios.

Entre las opciones de diseño y maquetación disponibles destacan:

* **Encabezado y pie de página**: Mediante editores de texto enriquecido (WYSIWYG), se pueden personalizar libremente la cabecera y el pie del formulario. Esto es especialmente útil para incluir el logotipo de la entidad, un título descriptivo, instrucciones iniciales de rellenado, textos legales (como la política de privacidad) o mensajes de información y agradecimiento en la parte inferior.

* **Secciones y contenedores visuales**: El formulario se estructura organizando los campos en diferentes **"Secciones"**, que actúan como contenedores visuales. Una sección puede incluir campos de uno o más bloques de datos. A nivel visual, estas secciones ofrecen mucha versatilidad:

  * **Formato de contenedor**: Pueden mostrarse de forma limpia como un panel simple (sin bordes) o en formato tarjeta (con borde y fondo diferenciado).
  
  * **Título**: Se puede definir si la sección muestra un título visible para estructurar el contenido o si queda oculto.

  * **Comportamiento colapsable (Acordeón)**: Las secciones pueden configurarse como paneles desplegables, permitiendo elegir si al cargar la página aparecen expandidas o contraídas por defecto. Esto resulta extremadamente útil en formularios largos para no abrumar al usuario, permitiéndole navegar progresivamente por bloques o revelar información opcional solo si interactúa con ella.

* **Configuración general de estilos**: Existen multitud de opciones para adaptar la apariencia del formulario y lograr que se integre a la perfección con la identidad visual corporativa:

  * **Botón de envío**: Personalización del texto del botón de envío de datos del formulario (por ejemplo: "Hacer donativo", "Inscribirme", "Firmar petición").

  * **Estructura en columnas**: Se puede definir el número máximo de columnas de cada sección para organizar los elementos internos de forma más eficiente y compacta en la pantalla, adaptándose siempre a los distintos tamaños de pantalla.

  * **Apariencia y diseño**: Control detallado sobre el esquema de colores, la tipografía, los tamaños de fuente, los bordes y los sombreados.
  
  * **Campos y etiquetas**: Personalización del diseño visual de los campos y la disposición de las etiquetas, incluyendo el soporte para **Etiquetas Flotantes** (animación donde la etiqueta se integra dentro del propio campo).

  * **Textos de estado y mensajes**: Permite personalizar los mensajes que verán los visitantes si intentan acceder al formulario cuando este no es público o cuando se envíen los datos si no hay ninguna redirección definida.

  * **CSS Personalizado**: Para necesidades más avanzadas, el sistema permite inyectar código CSS propio para aplicar ajustes de diseño a medida y sin límites. Esta opción solo se mostrará si el usuario es un Administrador.

### Paso 5: Resumen y publicación ###
Este es el último paso del asistente, destinado a revisar el resultado final, establecer la disponibilidad del formulario y elegir el método para compartirlo.

* **Estado del formulario**: Permite definir en qué situación se encuentra el formulario, controlando así el acceso y la recepción de respuestas:

  * **Borrador**: El formulario está en fase de preparación o edición. Todavía no es accesible públicamente y no acepta respuestas.

  * **Público**: El formulario está activo y publicado; por lo tanto, permite recibir y registrar respuestas de forma normal.

  * **Cerrado**: El formulario se desactiva y ya no acepta más respuestas (ideal cuando finaliza una campaña o se llena un aforo). 
  
* **Programación de la publicación**: Si el formulario se encuentra en estado "Público", de forma opcional se puede programar una **fecha y hora de inicio y de fin** de la publicación. Esto permite automatizar la apertura y cierre del formulario sin necesidad de intervención manual.

* **Bloqueo visual y de seguridad**: Cuando un formulario no sea **"Público"** o esté fuera de las fechas programadas para su apertura, el sistema bloqueará automáticamente su uso mostrando una capa superpuesta (*overlay*) transparente que impide rellenar los campos y enviar respuestas, acompañada de un mensaje central informativo indicando que el formulario está cerrado (cuyo título y texto son totalmente configurables desde el asistente de maquetación). En el caso de que se saltase esta restricción visual y se recibiese una respuesta, esta se guardará y marcará en el sistema, pero no se procesará en ningún caso.

* **Previsualización**: Antes de compartirlo, se dispone de un botón para previsualizar el formulario generado. Esta vista previa genera una réplica interactiva en un entorno seguro que desactiva los botones de envío reales (evitando generar registros de prueba en el CRM). Además, incluye una barra de herramientas superior que permite emular el estado del formulario, pudiendo comprobar de forma interactiva cómo se verá y cómo se comportará tanto cuando está activo como cuando no acepta respuestas.

* **Opciones de publicación**: Una vez listo, el sistema ofrece diferentes vías para difundir y hacer accesible el formulario:

  * **URL pública**: El CRM genera automáticamente un enlace accesible públicamente hacia el formulario alojado en el propio sistema. Es la opción ideal para compartirlo rápidamente sin tener que publicarlo en un sitio web externo.
  
  * **Incrustar (Iframe)**: Proporciona el código necesario para insertar el formulario publicado en el CRM directamente dentro de una página web externa, como la web corporativa de la entidad.
  
  * **Descargar código**: Permite descargar el código HTML generado para realizar inserciones manuales o integraciones más específicas en plataformas externas.

## Análisis y Métricas de Rendimiento
Además de capturar datos, el formulario web es una herramienta de medición de impacto. Desde la vista de detalle del formulario, se centraliza toda la información estadística y de rendimiento de la campaña:

* **Información general y disponibilidad**: Muestra de un vistazo el *Estado* actual (*Borrador*, *Público* o *Cerrado*), y si el formulario es público, refleja también las *Fechas de inicio y fin* programadas para su apertura y cierre automáticos.

* **Tráfico y métricas SEO**: Se contabilizan las *Visitas totales* que recibe el formulario. Asimismo, se registra el número de *Visitas bloqueadas*, que corresponden a intentos de acceso mediante la URL pública cuando el formulario se encuentra cerrado.

* **Calidad y filtrado**: Permite analizar la tasa de éxito y seguridad comparando el volumen de *Respuestas válidas* reales procesadas frente al número de *Respuestas Spam* que los sistemas de protección han detectado y bloqueado.

* **Fuentes de tráfico**: El sistema consolida y elabora un resumen de los dominios y orígenes (webs) desde los cuales se ha recibido alguna respuesta a ese formulario. Esto permite a la entidad identificar qué canales, páginas propias o dominios aliados están aportando más valor y resultados a la campaña.


## Gestión de Respuestas y Trazabilidad Total ##
A diferencia de sistemas anteriores, los Formularios Web Avanzados ofrecen una auditoría completa y detallada de todo lo que ocurre con la información recibida. El sistema se organiza en tres niveles de registro para garantizar una trazabilidad total:

* **Respuestas**: Desde la vista de detalle de un formulario se pueden consultar todas las respuestas que este ha recibido a lo largo del tiempo. Cada registro de respuesta guarda la información de forma íntegra e incluye datos técnicos y de contexto:

  * **Estado**: Indica la situación del ciclo de vida en la que se encuentra el envío (por ejemplo: *Pendiente*, *Procesando*, *En espera*, *Procesada*, *Rechazada*, *No deseada*, o *Error*). Cabe destacar que, dependiendo de la configuración y de los flujos del formulario, puede que no se utilicen todos los estados.
  
  * **Origen y contexto técnico**: Registra la URL exacta de la página desde la cual se envió el formulario, el navegador y sistema operativo del usuario, y su dirección IP.
  
  * **Control de seguridad**: Incluye el *hash* único de la respuesta, un identificador diseñado para detectar envíos repetidos en ventanas de tiempo cortas y evitar dobles procesamientos accidentales.
  
  * **Datos y ejecución**: Muestra un resumen con la respuesta original recibida debidamente formateada de forma legible, así como el registro de ejecución de las acciones y los mensajes de error en caso de que el procesado haya fallado.

* **Detalles de respuestas**: Cada respuesta cuenta con un listado estructurado de "detalles". Este componente guarda las respuestas de forma desglosada y está pensado específicamente para facilitar el análisis estadístico y la extracción de métricas. Además, para facilitar los informes y la comparativa de datos cruzados, los campos de valoración para encuestas (estrellas, emojis, NPS, etc.) normalizan su puntuación de forma automática y la guardan unificada en una escala del 0 al 100 en el campo *"Valor entero de la respuesta"*.

* **Vínculos (Trazabilidad en el CRM)**: Es el módulo encargado de documentar de forma granular qué impacto exacto ha tenido una respuesta dentro de la base de datos. Cada respuesta tiene uno o varios "Vínculos" asociados. Un vínculo informa de manera precisa sobre:

  * **Registro afectado**: A qué módulo y a qué registro concreto del CRM hace referencia (incluyendo un enlace directo para acceder a la ficha afectada).
  
  * **Acción realizada**: Especifica la operación exacta que el automatismo ejecutó sobre ese registro: *Creado*, *Actualizado*, *Ampliado* (solo se llenaron los campos que estaban en blanco) o *Ignorado*.
  
  * **Datos aplicados**: Muestra un desglose del subconjunto de datos que se aplicó a dicho registro, indicando los campos afectados, el valor que se recibió desde el formulario y cómo ha cambiado dicho valor en el CRM (el salto desde el valor que tenía originalmente hasta el nuevo valor asignado).

## Opciones Avanzadas
El nuevo sistema ofrece un grado de flexibilidad adicional para perfiles técnicos y administradores, permitiendo adaptar los formularios a casos de uso muy específicos que van más allá de la configuración estándar del asistente:

* **Extensibilidad mediante código**: El ecosistema de formularios no está limitado a lo preestablecido. Tanto las acciones (automatismos) como las validaciones están diseñadas con una arquitectura desacoplada que permite definirlas íntegramente por código. Esto resulta vital para entidades que requieran desarrollar reglas de negocio a medida o validaciones muy específicas y sumarlas al catálogo estándar de acciones de su CRM.
  * *Guía para desarrolladores*: El propio código del módulo incluye **múltiples plantillas de ejemplo** exhaustivas con documentación técnica detallada para cada tipo de acción, clase base e interfaz del sistema. Todas ellas están ubicadas en el directorio `modules/stic_AWF_Forms/actions/` y organizadas por tipo:
    * `Hook/ExampleAction.php` — Acción inmediata genérica (HookActionDefinition). Muestra todos los tipos de parámetros disponibles.
    * `Deferred/ExampleDeferredAction.php` — Acción diferida genérica sin dependencia de bloque de datos (DeferredActionDefinition + IDeferredAction).
    * `Deferred/ExampleDeferredDataBlockAction.php` — Acción diferida vinculada a un bloque de datos (DeferredDataBlockActionDefinition).
    * `Deferred/ExampleDeferredBeanAction.php` — Acción diferida sobre un registro guardado (DeferredBeanActionDefinition).
    * `Deferred/ExampleTerminalWebhookAction.php` — Acción diferida avanzada con interfaz de terminal y decodificación de webhooks (DeferredActionDefinition + ITerminalAction + IWebhookDecodable).
    * `Validator/ExampleValidatorAction.php` — Validador personalizado con lógica JS y backend (ValidatorActionDefinition).
    * `DataProvider/ExampleDataProviderAction.php` — Proveedor de datos dinámicos desde CRM (DataProviderActionDefinition).
    * `UI/ExampleUIAction.php` — Acción de frontend que inyecta JS/CSS en el formulario (UIActionDefinition + IFrontendAction).
    * `Group/ExampleGroupAction.php` — Grupo de acciones reutilizables (GroupActionDefinition).

* **Campos ocultos (vs Campos de servidor)**: Los formularios web avanzados incorporan los campos ocultos (`<input type="hidden">`). A diferencia de los "Campos de servidor" (que se configuran y viven de forma segura exclusivamente en el servidor del CRM), los campos ocultos sí se inyectan en el código HTML del formulario.
  * *Casos de uso*: Son ideales para capturar información de contexto dinámica que no requiere intervención visual del usuario, como leer parámetros pasados por la URL (ej: `?origen=newsletter_abril`), capturar identificadores de seguimiento, o permitir que un script externo de la web rellene el campo automáticamente antes de enviar la respuesta.

* **Edición HTML e inyección de nuevos campos**: El sistema genera un código HTML **limpio, indentado y optimizado para su legibilidad**, el cual puede ser descargado y editado libremente para su inserción en plataformas externas. Si se respeta la nomenclatura, el CRM entenderá perfectamente cualquier nuevo campo añadido manualmente en el código sin romper el formulario. Para añadir nuevos campos mapeados al CRM se usa el patrón `NombreBloque.NombreCampoCRM` y, para datos de uso exclusivo en la respuesta (campos no enlazados), el patrón `_detached.NombreBloque.NombreCampo`.
    * *Casos de uso*: Al generar un código limpio, resulta sencillo para un *webmaster* o desarrollador leerlo y modificarlo, aportando libertad total para crear diseños web a medida, maquetaciones altamente personalizadas, o inyectar campos interactivos por JavaScript.

## Próximamente (Evolución del sistema) ##
* **Archivos adjuntos**: Existirá la opción de que los usuarios puedan subir y adjuntar archivos o documentos digitales directamente a través del formulario.

* **Grupos de bloques de datos repetibles**: Se incorporará el concepto de "Grupo", un contenedor que agrupa uno o más bloques de datos relacionados entre sí. Su característica principal es que podrá definirse como "repetible", permitiendo que el conjunto de campos que contiene aparezca múltiples veces en un mismo formulario. Esta funcionalidad será ideal para simplificar operativas complejas, como recoger los datos de varios participantes a la vez en una única inscripción grupal.

* **Mayor flexibilidad en diseño y maquetación**: Se ampliarán las opciones visuales para organizar la información de forma más dinámica, permitiendo agrupar campos mediante pestañas, dividir formularios extensos en múltiples páginas y añadir otros elementos interactivos. El modelo de datos ya contempla estos contenedores, pendiente de completar su implementación en la capa de renderizado.

* **Condiciones lógicas avanzadas**: Se ampliará el soporte de condiciones lógicas con nuevos operadores (mayor que, menor que, contiene, etc.) y combinaciones múltiples, más allá de la igualdad estricta actual. La infraestructura base de evaluación condicional ya existe en el sistema, preparada para ser extendida.

<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3Agent extends \Google\Collection
{
  protected $collection_key = 'supportedLanguageCodes';
  protected $advancedSettingsType = GoogleCloudDialogflowCxV3AdvancedSettings::class;
  protected $advancedSettingsDataType = '';
  protected $answerFeedbackSettingsType = GoogleCloudDialogflowCxV3AgentAnswerFeedbackSettings::class;
  protected $answerFeedbackSettingsDataType = '';
  /**
   * @var string
   */
  public $avatarUri;
  protected $clientCertificateSettingsType = GoogleCloudDialogflowCxV3AgentClientCertificateSettings::class;
  protected $clientCertificateSettingsDataType = '';
  /**
   * @var string
   */
  public $defaultLanguageCode;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $enableMultiLanguageTraining;
  /**
   * @var bool
   */
  public $enableSpellCorrection;
  /**
   * @var bool
   */
  public $enableStackdriverLogging;
  protected $genAppBuilderSettingsType = GoogleCloudDialogflowCxV3AgentGenAppBuilderSettings::class;
  protected $genAppBuilderSettingsDataType = '';
  protected $gitIntegrationSettingsType = GoogleCloudDialogflowCxV3AgentGitIntegrationSettings::class;
  protected $gitIntegrationSettingsDataType = '';
  /**
   * @var bool
   */
  public $locked;
  /**
   * @var string
   */
  public $name;
  protected $personalizationSettingsType = GoogleCloudDialogflowCxV3AgentPersonalizationSettings::class;
  protected $personalizationSettingsDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $securitySettings;
  protected $speechToTextSettingsType = GoogleCloudDialogflowCxV3SpeechToTextSettings::class;
  protected $speechToTextSettingsDataType = '';
  /**
   * @var string
   */
  public $startFlow;
  /**
   * @var string[]
   */
  public $supportedLanguageCodes;
  protected $textToSpeechSettingsType = GoogleCloudDialogflowCxV3TextToSpeechSettings::class;
  protected $textToSpeechSettingsDataType = '';
  /**
   * @var string
   */
  public $timeZone;

  /**
   * @param GoogleCloudDialogflowCxV3AdvancedSettings
   */
  public function setAdvancedSettings(GoogleCloudDialogflowCxV3AdvancedSettings $advancedSettings)
  {
    $this->advancedSettings = $advancedSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AdvancedSettings
   */
  public function getAdvancedSettings()
  {
    return $this->advancedSettings;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AgentAnswerFeedbackSettings
   */
  public function setAnswerFeedbackSettings(GoogleCloudDialogflowCxV3AgentAnswerFeedbackSettings $answerFeedbackSettings)
  {
    $this->answerFeedbackSettings = $answerFeedbackSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AgentAnswerFeedbackSettings
   */
  public function getAnswerFeedbackSettings()
  {
    return $this->answerFeedbackSettings;
  }
  /**
   * @param string
   */
  public function setAvatarUri($avatarUri)
  {
    $this->avatarUri = $avatarUri;
  }
  /**
   * @return string
   */
  public function getAvatarUri()
  {
    return $this->avatarUri;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AgentClientCertificateSettings
   */
  public function setClientCertificateSettings(GoogleCloudDialogflowCxV3AgentClientCertificateSettings $clientCertificateSettings)
  {
    $this->clientCertificateSettings = $clientCertificateSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AgentClientCertificateSettings
   */
  public function getClientCertificateSettings()
  {
    return $this->clientCertificateSettings;
  }
  /**
   * @param string
   */
  public function setDefaultLanguageCode($defaultLanguageCode)
  {
    $this->defaultLanguageCode = $defaultLanguageCode;
  }
  /**
   * @return string
   */
  public function getDefaultLanguageCode()
  {
    return $this->defaultLanguageCode;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param bool
   */
  public function setEnableMultiLanguageTraining($enableMultiLanguageTraining)
  {
    $this->enableMultiLanguageTraining = $enableMultiLanguageTraining;
  }
  /**
   * @return bool
   */
  public function getEnableMultiLanguageTraining()
  {
    return $this->enableMultiLanguageTraining;
  }
  /**
   * @param bool
   */
  public function setEnableSpellCorrection($enableSpellCorrection)
  {
    $this->enableSpellCorrection = $enableSpellCorrection;
  }
  /**
   * @return bool
   */
  public function getEnableSpellCorrection()
  {
    return $this->enableSpellCorrection;
  }
  /**
   * @param bool
   */
  public function setEnableStackdriverLogging($enableStackdriverLogging)
  {
    $this->enableStackdriverLogging = $enableStackdriverLogging;
  }
  /**
   * @return bool
   */
  public function getEnableStackdriverLogging()
  {
    return $this->enableStackdriverLogging;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AgentGenAppBuilderSettings
   */
  public function setGenAppBuilderSettings(GoogleCloudDialogflowCxV3AgentGenAppBuilderSettings $genAppBuilderSettings)
  {
    $this->genAppBuilderSettings = $genAppBuilderSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AgentGenAppBuilderSettings
   */
  public function getGenAppBuilderSettings()
  {
    return $this->genAppBuilderSettings;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AgentGitIntegrationSettings
   */
  public function setGitIntegrationSettings(GoogleCloudDialogflowCxV3AgentGitIntegrationSettings $gitIntegrationSettings)
  {
    $this->gitIntegrationSettings = $gitIntegrationSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AgentGitIntegrationSettings
   */
  public function getGitIntegrationSettings()
  {
    return $this->gitIntegrationSettings;
  }
  /**
   * @param bool
   */
  public function setLocked($locked)
  {
    $this->locked = $locked;
  }
  /**
   * @return bool
   */
  public function getLocked()
  {
    return $this->locked;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AgentPersonalizationSettings
   */
  public function setPersonalizationSettings(GoogleCloudDialogflowCxV3AgentPersonalizationSettings $personalizationSettings)
  {
    $this->personalizationSettings = $personalizationSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AgentPersonalizationSettings
   */
  public function getPersonalizationSettings()
  {
    return $this->personalizationSettings;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setSecuritySettings($securitySettings)
  {
    $this->securitySettings = $securitySettings;
  }
  /**
   * @return string
   */
  public function getSecuritySettings()
  {
    return $this->securitySettings;
  }
  /**
   * @param GoogleCloudDialogflowCxV3SpeechToTextSettings
   */
  public function setSpeechToTextSettings(GoogleCloudDialogflowCxV3SpeechToTextSettings $speechToTextSettings)
  {
    $this->speechToTextSettings = $speechToTextSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3SpeechToTextSettings
   */
  public function getSpeechToTextSettings()
  {
    return $this->speechToTextSettings;
  }
  /**
   * @param string
   */
  public function setStartFlow($startFlow)
  {
    $this->startFlow = $startFlow;
  }
  /**
   * @return string
   */
  public function getStartFlow()
  {
    return $this->startFlow;
  }
  /**
   * @param string[]
   */
  public function setSupportedLanguageCodes($supportedLanguageCodes)
  {
    $this->supportedLanguageCodes = $supportedLanguageCodes;
  }
  /**
   * @return string[]
   */
  public function getSupportedLanguageCodes()
  {
    return $this->supportedLanguageCodes;
  }
  /**
   * @param GoogleCloudDialogflowCxV3TextToSpeechSettings
   */
  public function setTextToSpeechSettings(GoogleCloudDialogflowCxV3TextToSpeechSettings $textToSpeechSettings)
  {
    $this->textToSpeechSettings = $textToSpeechSettings;
  }
  /**
   * @return GoogleCloudDialogflowCxV3TextToSpeechSettings
   */
  public function getTextToSpeechSettings()
  {
    return $this->textToSpeechSettings;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3Agent::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3Agent');

Checklist to keep track of which parts of the Raw API are covered by the high-level API

Raw.Asynchronous
================
+ [X] 2016-10-25 solveAsyncCancel :: MonadIO m => AsyncSolver -> m CBool
+ [X] 2016-10-26 solveAsyncGet :: MonadIO m => AsyncSolver -> Ptr SolveResult -> m CBool
+ [X] 2016-10-25 solveAsyncWait :: MonadIO m => AsyncSolver -> CDouble -> Ptr CBool 

Raw.Basic
=========
+ [X] 2016-10-25 errorCode :: MonadIO m => m ClingoError
+ [X] 2016-10-25 errorMessage :: MonadIO m => m CString
+ [X] 2016-10-26 errorString :: MonadIO m => ClingoError -> m CString
+ [X] 2016-10-26 setError :: MonadIO m => ClingoError -> CString -> m ()
+ [X] 2016-10-25 version :: MonadIO m => Ptr CInt -> Ptr CInt -> Ptr CInt -> m ()
+ [X] 2016-10-26 warningString :: MonadIO m => ClingoWarning -> m CString

Raw.Configuration
=================
+ [ ] configurationArrayAt :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr Identifier -> m CBool
+ [ ] configurationArraySize :: MonadIO m => Configuration -> Identifier -> Word64 -> Ptr CSize -> m CBool
+ [ ] configurationDescription :: MonadIO m => Configuration -> Identifier -> Ptr CString -> m CBool
+ [ ] configurationMapAt :: MonadIO m => Configuration -> Identifier -> CString -> Ptr Identifier -> m CBool
+ [ ] configurationMapSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [ ] configurationMapSubkeyName :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr CString -> m CBool
+ [ ] configurationRoot :: MonadIO m => Configuration -> Ptr Identifier -> m CBool
+ [ ] configurationType :: MonadIO m => Configuration -> Identifier -> Ptr ConfigurationType -> m CBool
+ [ ] configurationValueGet :: MonadIO m => Configuration -> Identifier -> CString -> CSize -> m CBool
+ [ ] configurationValueGetSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [ ] configurationValueIsAssigned :: MonadIO m => Configuration -> Identifier -> Ptr CBool -> m CBool
+ [ ] configurationValueSet :: MonadIO m => Configuration -> Identifier -> CString -> m CBool

Raw.Control
===========
+ [X] 2016-10-27 controlAdd :: MonadIO m => Control -> CString -> Ptr CString -> CSize 
+ [ ] controlAssignExternal :: MonadIO m => Control -> Symbol -> TruthValue 
+ [X] 2016-10-26 controlBackend :: MonadIO m => Control -> Ptr Backend -> m CBool
+ [ ] controlClaspFacade :: MonadIO m => Control -> Ptr (Ptr ()) -> m CBool
+ [X] 2016-10-26 controlCleanup :: MonadIO m => Control -> m CBool
+ [X] 2016-10-26 controlConfiguration :: MonadIO m => Control -> Ptr Configuration
+ [X] 2016-10-25 controlFree :: MonadIO m => Control -> m ()
+ [ ] controlGetConst :: MonadIO m => Control -> CString -> Ptr Symbol -> m CBool
+ [X] 2016-10-26 controlGround :: MonadIO m => Control -> Ptr Part -> CSize 
+ [ ] controlHasConst :: MonadIO m => Control -> CString -> Ptr CBool -> m CBool
+ [X] 2016-10-26 controlInterrupt :: MonadIO m => Control -> m ()
+ [X] 2016-10-25 controlLoad :: MonadIO m => Control -> CString -> m CBool
+ [X] 2016-10-25 controlNew :: MonadIO m => Ptr CString -> CSize -> FunPtr (Logger a) 
+ [X] 2016-10-26 controlProgramBuilder :: MonadIO m => Control -> Ptr ProgramBuilder
+ [ ] controlRegisterObserver :: MonadIO m => Control 
+ [ ] controlRegisterPropagator :: MonadIO m => Control -> Ptr (Propagator a) 
+ [ ] controlReleaseExternal :: MonadIO m => Control -> Symbol -> m CBool
+ [X] 2016-10-26 controlSolve :: MonadIO m => Control -> FunPtr (CallbackModel a) -> Ptr a 
+ [X] 2016-10-26 controlSolveAsync :: MonadIO m => Control -> FunPtr (CallbackModel a) 
+ [X] 2016-10-26 controlSolveIter :: MonadIO m => Control -> Ptr SymbolicLiteral -> CSize 
+ [X] 2016-10-26 controlStatistics :: MonadIO m => Control -> Ptr Statistics -> m CBool
+ [X] 2016-10-26 controlSymbolicAtoms :: MonadIO m => Control -> Ptr SymbolicAtoms
+ [X] 2016-10-26 controlTheoryAtoms :: MonadIO m => Control -> Ptr TheoryAtoms
+ [ ] controlUseEnumAssumption :: MonadIO m => Control -> CBool -> m CBool

Raw.Iterative
=============
+ [X] 2016-10-25 solveIterativelyClose :: MonadIO m 
+ [X] 2016-10-26 solveIterativelyGet :: MonadIO m 
+ [X] 2016-10-25 solveIterativelyNext :: MonadIO m 

Raw.Model
=========
+ [ ] modelContains :: MonadIO m => Model -> Symbol -> Ptr CBool -> m CBool
+ [ ] modelContext :: MonadIO m => Model -> Ptr SolveControl -> m CBool
+ [ ] modelCost :: MonadIO m => Model -> Ptr Int64 -> CSize -> m CBool
+ [ ] modelCostSize :: MonadIO m => Model -> Ptr CSize -> m CBool
+ [ ] modelNumber :: MonadIO m => Model -> Ptr Word64 -> m CBool
+ [ ] modelOptimalityProven :: MonadIO m => Model -> Ptr CBool -> m CBool
+ [ ] modelSymbols :: MonadIO m => Model -> ShowFlag -> Ptr Symbol -> CSize 
+ [ ] modelSymbolsSize :: MonadIO m => Model -> ShowFlag -> Ptr CSize -> m CBool
+ [ ] modelType :: MonadIO m => Model -> ModelType -> m CBool
+ [ ] solveControlAddClause :: MonadIO m => SolveControl -> Ptr SymbolicLiteral 
+ [ ] solveControlThreadId :: MonadIO m => SolveControl -> Ptr Identifier 

Raw.ProgramBuilding
===================
+ [ ] backendAcycEdge :: MonadIO m => Backend -> CInt -> CInt -> Ptr Literal -> CSize -> m CBool
+ [ ] backendAddAtom :: MonadIO m => Backend -> Ptr Atom -> m CBool
+ [ ] backendAssume :: MonadIO m => Backend -> Ptr Literal -> CSize -> m CBool
+ [ ] backendBuilderAdd :: MonadIO m => ProgramBuilder -> Ptr AstStatement -> m CBool
+ [ ] backendBuilderBegin :: MonadIO m => ProgramBuilder -> m CBool
+ [ ] backendBuilderEnd :: MonadIO m => ProgramBuilder -> m CBool
+ [ ] backendExternal :: MonadIO m => Backend -> Atom -> ExternalType -> m CBool
+ [ ] backendHeuristic :: MonadIO m => Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> m CBool
+ [ ] backendMinimize :: MonadIO m => Backend -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
+ [ ] backendProject :: MonadIO m => Backend -> Ptr Atom -> CSize -> m CBool
+ [ ] backendRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> m CBool
+ [ ] backendWeightRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool

Raw.Propagation
===============
+ [ ] assignmentDecision :: MonadIO m => Assignment -> Word32 -> Ptr Literal -> m CBool
+ [ ] assignmentDecisionLevel :: MonadIO m => Assignment -> m Word32
+ [ ] assignmentHasConflict :: MonadIO m => Assignment -> m CBool
+ [ ] assignmentHasLiteral :: MonadIO m => Assignment -> Literal -> m CBool
+ [ ] assignmentIsFalse :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentIsFixed :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentIsTrue :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentLevel :: MonadIO m => Assignment -> Literal -> Ptr Word32 -> m CBool
+ [ ] assignmentTruthValue :: MonadIO m => Assignment -> Literal -> Ptr TruthValue -> m CBool
+ [ ] propagateControlAddClause :: MonadIO m => PropagateControl -> Ptr Literal -> CSize -> ClauseType -> Ptr CBool -> m CBool
+ [ ] propagateControlAddLiteral :: MonadIO m => PropagateControl -> Ptr Literal -> m CBool
+ [ ] propagateControlAddWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [ ] propagateControlAssignment :: MonadIO m => PropagateControl -> m Assignment
+ [ ] propagateControlHasWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [ ] propagateControlPropagate :: MonadIO m => PropagateControl -> Ptr CBool -> m CBool
+ [ ] propagateControlRemoveWatch :: MonadIO m => PropagateControl -> Literal -> m ()
+ [ ] propagateControlThreadId :: MonadIO m => PropagateControl -> m Identifier
+ [ ] propagateInitAddWatch :: MonadIO m => PropagateInit -> Literal -> m CBool
+ [ ] propagateInitNumberOfThreads :: MonadIO m => PropagateInit -> m CInt
+ [ ] propagateInitSolverLiteral :: MonadIO m => PropagateInit -> Literal -> Ptr Literal -> m CBool
+ [ ] propagateInitSymbolicAtoms :: MonadIO m => PropagateInit -> Ptr SymbolicAtoms -> m CBool
+ [ ] propagateInitTheoryAtoms :: MonadIO m => PropagateInit -> Ptr TheoryAtoms -> m CBool

Raw.Statistics
==============
+ [X] 2016-10-26 statisticsArrayAt :: MonadIO m => Statistics -> Word64 -> CSize 
+ [X] 2016-10-26 statisticsArraySize :: MonadIO m => Statistics -> Word64 -> Ptr Word64 
+ [X] 2016-10-26 statisticsMapAt :: MonadIO m => Statistics -> Word64 -> Ptr CString 
+ [X] 2016-10-26 statisticsMapSize :: MonadIO m => Statistics -> Word64 -> Ptr CSize 
+ [X] 2016-10-26 statisticsMapSubkeyName :: MonadIO m => Statistics -> Word64 -> CSize 
+ [X] 2016-10-26 statisticsRoot :: MonadIO m => Statistics -> Ptr Word64 -> m CBool
+ [X] 2016-10-26 statisticsType :: MonadIO m => Statistics -> Word64 -> Ptr StatisticsType 
+ [X] 2016-10-26 statisticsValueGet :: MonadIO m => Statistics -> Word64 -> Ptr CString 

Raw.Symbol
==========
+ [o] addString :: MonadIO m => CString -> Ptr CString -> m CBool
+ [X] 2016-10-26 parseTerm :: MonadIO m => CString -> FunPtr (Logger a) -> Ptr a -> CUInt 
+ [X] 2016-10-25 signatureArity :: Signature -> Word32
+ [X] 2016-10-26 signatureCreate :: MonadIO m => CString -> Word32 -> CBool -> Ptr Signature 
+ [X] 2016-10-25 signatureHash :: Signature -> CSize
+ [X] 2016-10-25 signatureIsEqualTo :: Signature -> Signature -> CBool
+ [X] 2016-10-25 signatureIsLessThan :: Signature -> Signature -> CBool
+ [X] 2016-10-25 signatureIsNegative :: Signature -> CBool
+ [X] 2016-10-25 signatureIsPositive :: Signature -> CBool
+ [X] 2016-10-25 signatureName :: Signature -> CString
+ [X] 2016-10-26 symbolArguments :: MonadIO m => Symbol -> Ptr (Ptr Symbol) -> Ptr CSize 
+ [X] 2016-10-26 symbolCreateFunction :: MonadIO m => CString -> Ptr Symbol -> CSize -> CBool 
+ [X] 2016-10-26 symbolCreateId :: MonadIO m => CString -> CBool -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateInfimum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolCreateNumber :: MonadIO m => CInt -> Ptr Symbol -> m ()
+ [X] 2016-10-26 symbolCreateString :: MonadIO m => CString -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateSupremum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolHash :: Symbol -> CSize
+ [X] 2016-10-25 symbolIsEqualTo :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsLessThan :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsNegative :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-25 symbolIsPositive :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-26 symbolName :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [X] 2016-10-26 symbolNumber :: MonadIO m => Symbol -> Ptr CInt -> m CBool
+ [X] 2016-10-26 symbolString :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [X] 2016-10-26 symbolSymbolToString :: MonadIO m => Symbol -> Ptr CChar -> m CBool
+ [X] 2016-10-26 symbolSymbolToStringSize :: MonadIO m => Symbol -> Ptr CSize -> m CBool
+ [X] 2016-10-25 symbolType :: Symbol -> SymbolType
